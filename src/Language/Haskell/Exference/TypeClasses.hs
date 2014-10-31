{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.TypeClasses
  ( HsTypeClass (..)
  , HsInstance (..)
  , HsConstraint (..)
  , StaticClassEnv (..)
  , QueryClassEnv ( qClassEnv_env
                  , qClassEnv_constraints
                  , qClassEnv_inflatedConstraints
                  , qClassEnv_varConstraints )
  , constraintApplySubsts
  , mkStaticClassEnv
  , mkQueryClassEnv
  , inflateHsConstraints
  , constraintMapTypes
  , constraintContainsVariables
  , unknownTypeClass
  , inflateInstances
  )
where



import Language.Haskell.Exference.Type
-- import Language.Haskell.Exference.Internal.Unify
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Foldable ( fold, foldMap )
import Data.List ( intercalate )
import Debug.Trace
import Data.Maybe ( fromMaybe )
import Control.Applicative ( (<$>) )
import Control.Monad ( mplus, guard )
import Control.Monad.Identity ( Identity(runIdentity) )

import Control.DeepSeq.Generics
import GHC.Generics

import Debug.Hood.Observe


data HsTypeClass = HsTypeClass
  { tclass_name :: String
  , tclass_params :: [TVarId]
  , tclass_constraints :: [HsConstraint]
  }
  deriving (Eq, Show, Ord, Generic)

data HsInstance = HsInstance
  { instance_constraints :: [HsConstraint]
  , instance_tclass :: HsTypeClass
  , instance_params :: [HsType]
  }
  deriving (Eq, Show, Ord, Generic)

data HsConstraint = HsConstraint
  { constraint_tclass :: HsTypeClass
  , constraint_params :: [HsType]
  }
  deriving (Eq, Ord, Generic)

data StaticClassEnv = StaticClassEnv
  { sClassEnv_tclasses :: [HsTypeClass]
  , sClassEnv_instances :: M.Map String [HsInstance]
  }
  deriving (Show, Generic)

data QueryClassEnv = QueryClassEnv
  { qClassEnv_env :: StaticClassEnv
  , qClassEnv_constraints :: S.Set HsConstraint
  , qClassEnv_inflatedConstraints :: S.Set HsConstraint
  , qClassEnv_varConstraints :: M.Map TVarId (S.Set HsConstraint)
  }
  deriving (Generic)

instance NFData HsTypeClass    where rnf = genericRnf
instance NFData HsInstance     where rnf = genericRnf
instance NFData HsConstraint   where rnf = genericRnf
instance NFData StaticClassEnv where rnf = genericRnf
instance NFData QueryClassEnv  where rnf = genericRnf

instance Show HsConstraint where
  show (HsConstraint c ps) = unwords $ tclass_name c : map show ps

instance Show QueryClassEnv where
  show (QueryClassEnv _ cs _ _) = "(QueryClassEnv _ " ++ show cs ++ " _)"
instance Observable HsConstraint where
  observer x = observeOpaque (show x) x

instance Observable QueryClassEnv where
  observer x = observeOpaque (show x) x

instance Observable HsInstance where
  observer x = observeOpaque (show x) x

constraintMapTypes :: (HsType -> HsType) -> HsConstraint -> HsConstraint
constraintMapTypes f (HsConstraint a ts) = HsConstraint a (map f ts)


mkStaticClassEnv :: [HsTypeClass] -> [HsInstance] -> StaticClassEnv
mkStaticClassEnv tclasses insts = StaticClassEnv tclasses (helper insts)
  where
    helper :: [HsInstance] -> M.Map String [HsInstance]
    helper is = M.fromListWith (++)
              $ [ (tclass_name $ instance_tclass i, [i]) | i <- is ]

mkQueryClassEnv :: StaticClassEnv -> [HsConstraint] -> QueryClassEnv
mkQueryClassEnv sClassEnv constrs = QueryClassEnv {
  qClassEnv_env = sClassEnv,
  qClassEnv_constraints = csSet,
  qClassEnv_inflatedConstraints = inflateHsConstraints csSet,
  qClassEnv_varConstraints = helper constrs
}
  where
    csSet = S.fromList constrs
    helper :: [HsConstraint] -> M.Map TVarId (S.Set HsConstraint)
    helper cs =
      let ids :: S.Set TVarId
          ids = fold $ freeVars <$> (constraint_params =<< cs)
      in M.fromSet (flip filterHsConstraintsByVarId
                    $ inflateHsConstraints csSet) ids

constraintApplySubsts :: Substs -> HsConstraint -> HsConstraint
constraintApplySubsts ss (HsConstraint c ps) =
  HsConstraint c $ map (applySubsts ss) ps

inflateHsConstraints :: S.Set HsConstraint -> S.Set HsConstraint
inflateHsConstraints = inflate (S.fromList . f)
  where
    f :: HsConstraint -> [HsConstraint]
    f (HsConstraint (HsTypeClass _ ids constrs) ps) =
      map (constraintApplySubsts $ M.fromList $ zip ids ps) constrs

filterHsConstraintsByVarId :: TVarId
                           -> S.Set HsConstraint
                           -> S.Set HsConstraint
filterHsConstraintsByVarId i = S.filter
                             $ any (containsVar i) . constraint_params

-- uses f to find new elements. adds these new elements, and recursively
-- tried to find even more elements. will not terminate if there are cycles
-- in the application of f
inflate :: (Ord a, Show a) => (a -> S.Set a) -> S.Set a -> S.Set a
inflate f = fold . S.fromList . iterateWhileNonempty (foldMap f)
  where
    iterateWhileNonempty g x = if S.null x
      then []
      else x : iterateWhileNonempty g (g x)

constraintContainsVariables :: HsConstraint -> Bool
constraintContainsVariables = any ((-1/=).largestId) . constraint_params

-- TODO: it probably is a bad idea to have any unknown type class mapped to
--       this, as they might unify at some point.. even if they distinct.
unknownTypeClass :: HsTypeClass
unknownTypeClass = HsTypeClass "EXFUnknownTC" [] []

inflateInstances :: [HsInstance] -> [HsInstance]
inflateInstances is = S.toList $ S.unions $ map (S.fromList . f) is
  where
    f :: HsInstance -> [HsInstance]
    f i@(HsInstance iconstrs tclass iparams)
      | (HsTypeClass _ tparams tconstrs) <- tclass
      , substs <- M.fromList $ zip tparams iparams
      = let 
          g :: HsConstraint -> HsInstance
          g (HsConstraint ctclass cparams) =
            HsInstance iconstrs ctclass $ map (applySubsts substs) cparams
        in i : concatMap (f.g) tconstrs

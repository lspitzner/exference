module TypeClasses
  ( HsTypeClass (..)
  , HsInstance (..)
  , Constraint (..)
  , StaticContext (..)
  , DynContext ( dynContext_context
               , dynContext_constraints
               , dynContext_varConstraints )
  , constraintApplySubsts
  , mkDynContext
  , inflateConstraints
  , constraintMapTypes
  )
where



import Type
import Unify
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Foldable ( fold, foldMap )
import Data.List ( intercalate )
import Debug.Trace
import Data.Maybe ( fromMaybe )
import Control.Applicative ( (<$>) )
import Control.Monad ( mplus, guard )
import Data.Functor.Identity ( Identity(runIdentity) )

import Debug.Hood.Observe


data HsTypeClass = HsTypeClass
  { tclass_name :: String
  , tclass_params :: [TVarId]
  , tclass_constraints :: [Constraint]
  }
  deriving (Eq, Show, Ord)

data HsInstance = HsInstance
  { instance_constraints :: [Constraint]
  , instance_tclass :: HsTypeClass
  , instance_params :: [HsType]
  }
  deriving (Eq, Show, Ord)

data Constraint = Constraint
  { constraint_tclass :: HsTypeClass
  , constraint_params :: [HsType]
  }
  deriving (Eq, Ord)

data StaticContext = StaticContext
  { context_tclasses :: [HsTypeClass]
  , context_instances :: [HsInstance]
  }
  deriving Show

data DynContext = DynContext
  { dynContext_context :: StaticContext
  , dynContext_constraints :: S.Set Constraint
  , dynContext_varConstraints :: M.Map TVarId (S.Set Constraint)
  }

instance Show Constraint where
  show (Constraint c ps) = unwords $ tclass_name c : map show ps

instance Show DynContext where
  show (DynContext _ cs _) = "(DynContext _ " ++ show cs ++ " _)"
instance Observable Constraint where
  observer x = observeOpaque (show x) x

instance Observable DynContext where
  observer x = observeOpaque (show x) x

constraintMapTypes :: (HsType -> HsType) -> Constraint -> Constraint
constraintMapTypes f (Constraint a ts) = Constraint a (map f ts)


mkDynContext :: StaticContext -> [Constraint] -> DynContext
mkDynContext staticContext constrs = DynContext {
  dynContext_context = staticContext,
  dynContext_constraints = csSet,
  dynContext_varConstraints = helper constrs
}
  where
    csSet = S.fromList constrs
    helper :: [Constraint] -> M.Map TVarId (S.Set Constraint)
    helper cs =
      let ids :: S.Set TVarId
          ids = fold $ freeVars <$> (constraint_params =<< cs)
      in M.fromSet (flip filterConstraintsByVarId
                    $ inflateConstraints staticContext csSet) ids

constraintApplySubsts :: Substs -> Constraint -> Constraint
constraintApplySubsts ss (Constraint c ps) =
  Constraint c $ map (applySubsts ss) ps

inflateConstraints :: StaticContext -> S.Set Constraint -> S.Set Constraint
inflateConstraints _context = inflate (S.fromList . f)
  where
    f :: Constraint -> [Constraint]
    f (Constraint (HsTypeClass _ ids constrs) ps) =
      map (constraintApplySubsts $ M.fromList $ zip ids ps) constrs

filterConstraintsByVarId :: TVarId -> S.Set Constraint -> S.Set Constraint
filterConstraintsByVarId i = S.filter $ any (containsVar i) . constraint_params

constraintMatches :: DynContext -> TVarId -> HsType -> Bool
constraintMatches dcontext constrVar providedType =
  let contextConstraints  = dynContext_constraints dcontext
      relevantConstraints = fromMaybe S.empty
                          $ M.lookup constrVar
                          $ dynContext_varConstraints dcontext
      wantedConstraints   = S.map
            (constraintApplySubsts $ M.singleton constrVar providedType)
            relevantConstraints
  in S.isSubsetOf wantedConstraints
    $ inflateConstraints
        (dynContext_context dcontext)
        contextConstraints
{-
 problem:
  given a set of constraints C over type variables a,b,c,..
    (for example: C={Monad m, Num a, Num b, Ord b})
  for each tuple of variables (v1,v2),
  can v1 be replaced by v2 without breaking the constraints?
  i.e. are the constraints for v1 a subset of the
    constraints for v2?
-}

-- uses f to find new elements. adds these new elements, and recursively
-- tried to find even more elements. will not terminate if there are cycles
-- in the application of f
inflate :: (Ord a, Show a) => (a -> S.Set a) -> S.Set a -> S.Set a
inflate f = fold . S.fromList . iterateWhileNonempty (foldMap f)
  where
    iterateWhileNonempty g x = if S.null x
      then []
      else x : iterateWhileNonempty g (g x)

dynContextAddConstraints :: [Constraint] -> DynContext -> DynContext
dynContextAddConstraints cs (DynContext a b _) =
  mkDynContext a (cs ++ S.toList b)

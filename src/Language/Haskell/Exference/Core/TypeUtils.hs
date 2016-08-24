{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Language.Haskell.Exference.Core.TypeUtils
  ( incVarIds
  , largestId
  , largestSubstsId
  , forallify -- unused atm
  , mkStaticClassEnv
  , constraintMapTypes
  , constraintContainsVariables
  , unknownTypeClass
  , inflateInstances
  , splitArrowResultParams
  )
where



import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IntMap
import Data.Char ( ord, chr, isLower, isUpper )
import Control.Applicative ( (<$>), (<*>), (*>), (<*) )
import Data.Maybe ( maybeToList, fromMaybe )
import Text.Printf ( printf )

import Control.Monad.State.Strict
import Control.DeepSeq.Generics
import GHC.Generics

import Language.Haskell.Exference.Core.Types
-- import Language.Haskell.Exference.Core.Internal.Unify
import Data.List ( intercalate, find )
import Control.Monad ( mplus, guard )
import Control.Monad.Identity ( Identity(runIdentity) )

import Control.Monad.Trans.MultiState ( MonadMultiState(..) )
import Control.Monad.Trans.MultiRWS

import Control.Lens ( ala )

import Debug.Hood.Observe
import Debug.Trace



-- binds everything in Foralls, so there are no free variables anymore.
forallify :: HsType -> HsType
forallify t = case t of
  TypeForall is cs t' -> TypeForall (S.toList frees++is) cs t'
  _                   -> TypeForall (S.toList frees) [] t
 where frees = freeVars t

incVarIds :: (TVarId -> TVarId) -> HsType -> HsType
incVarIds f (TypeVar i) = TypeVar (f i)
incVarIds f (TypeArrow t1 t2) = TypeArrow (incVarIds f t1) (incVarIds f t2)
incVarIds f (TypeApp t1 t2) = TypeApp (incVarIds f t1) (incVarIds f t2)
incVarIds f (TypeForall is cs t) = TypeForall
                                     (f <$> is)
                                     (g <$> cs) 
                                     (incVarIds f t)
  where
    g (HsConstraint cls params) = HsConstraint cls (incVarIds f <$> params)
incVarIds _ t = t

largestId :: HsType -> TVarId
largestId (TypeVar i)       = i
largestId (TypeConstant _)  = -1
largestId (TypeCons _)      = -1
largestId (TypeArrow t1 t2) = largestId t1 `max` largestId t2
largestId (TypeApp t1 t2)   = largestId t1 `max` largestId t2
largestId (TypeForall _ _ t)  = largestId t

largestSubstsId :: Substs -> TVarId
largestSubstsId = IntMap.foldl' (\a b -> a `max` largestId b) 0

constraintMapTypes :: (HsType -> HsType) -> HsConstraint -> HsConstraint
constraintMapTypes f (HsConstraint a ts) = HsConstraint a (map f ts)


mkStaticClassEnv :: [HsTypeClass] -> [HsInstance] -> StaticClassEnv
mkStaticClassEnv tclasses insts = StaticClassEnv tclasses (helper insts)
  where
    helper :: [HsInstance] -> M.Map QualifiedName [HsInstance]
    helper is = M.fromListWith (++)
              $ [ (tclass_name $ instance_tclass i, [i]) | i <- is ]

constraintContainsVariables :: HsConstraint -> Bool
constraintContainsVariables = any ((-1/=).largestId) . constraint_params

-- TODO: it probably is a bad idea to have any unknown type class mapped to
--       this, as they might unify at some point.. even if they distinct.
unknownTypeClass :: HsTypeClass
unknownTypeClass = HsTypeClass qid [] []
  where
    qid = QualifiedName [] "EXFUnknownTC"
  

inflateInstances :: [HsInstance] -> [HsInstance]
inflateInstances = ala S.fromList id . concat . takeWhile (not . null) . iterate (concatMap f)
  where
    f :: HsInstance -> [HsInstance]
    f (HsInstance iconstrs tclass iparams)
      | (HsTypeClass _ tparams tconstrs) <- tclass
      , substs <- IntMap.fromList $ zip tparams iparams
      = let 
          g :: HsConstraint -> HsInstance
          g (HsConstraint ctclass cparams) =
            HsInstance iconstrs ctclass $ map (snd . applySubsts substs) cparams
        in map g tconstrs

splitArrowResultParams :: HsType -> (HsType, [HsType], [TVarId], [HsConstraint])
splitArrowResultParams t
  | TypeArrow t1 t2 <- t
  , (rt,pts,fvs,cs) <- splitArrowResultParams t2
  = (rt, t1:pts, fvs, cs)
  | TypeForall vs cs t1 <- t
  , (rt, pts, fvs, cs') <- splitArrowResultParams t1
  = (rt, pts, vs++fvs, cs++cs')
  | otherwise
  = (t, [], [], [])

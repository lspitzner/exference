{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Language.Haskell.Exference.Core.TypeUtils
  ( reduceIds
  , incVarIds
  , largestId
  , distinctify
  , arrowDepth
  , badReadVar
  , largestSubstsId
  , forallify -- unused atm
  , mkStaticClassEnv
  , constraintMapTypes
  , constraintContainsVariables
  , unknownTypeClass
  , inflateInstances
  , splitArrowResultParams
  , emptyQNameIndex
  , getOrCreateQNameId
  , findQName
  , lookupQNameId
  , withQNameIndex
  , showQNameIndex
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



emptyQNameIndex :: QNameIndex
emptyQNameIndex = QNameIndex 0 M.empty M.empty

getOrCreateQNameId :: MonadMultiState QNameIndex m
                   => QualifiedName -> m QNameId
getOrCreateQNameId name = do
  QNameIndex next indA indB <- mGet
  case M.lookup name indA of
    Nothing -> do
      mSet $ QNameIndex (next+1)
                        (M.insert name next indA)
                        (M.insert next name indB)
      return next
    Just i ->
      return i

findQName :: MonadMultiState QNameIndex m
          => (QualifiedName -> Bool)
          -> m (Maybe QNameId)
findQName p = do
  QNameIndex _ indA _ <- mGet
  return $ snd <$> find (\(qname, _) -> p qname) (M.toList indA)

withQNameIndex :: Monad m => MultiRWST r w (QNameIndex ': ss) m a -> MultiRWST r w ss m a
withQNameIndex = withMultiStateA emptyQNameIndex

showQNameIndex :: MonadMultiState QNameIndex m => m [String]
showQNameIndex = do
  QNameIndex _ _ indB <- mGet
  return $ [ printf "% 5d %s" k (show v) | (k,v) <- M.toAscList indB ]

badReadVar :: String -> TVarId
badReadVar [c] = ord c - ord 'a'
badReadVar _ = error "badReadVar: that's why it is called badReadVar"

arrowDepth :: HsType -> Int
arrowDepth (TypeVar _)      = 1
arrowDepth (TypeConstant _) = 1
arrowDepth (TypeCons _)     = 1
arrowDepth (TypeArrow _ t)  = 1 + arrowDepth t
arrowDepth (TypeApp _ _)    = 1
arrowDepth (TypeForall _ _ t) = arrowDepth t

-- binds everything in Foralls, so there are no free variables anymore.
forallify :: HsType -> HsType
forallify t = case t of
  TypeForall is cs t' -> TypeForall (S.toList frees++is) cs t'
  _                   -> TypeForall (S.toList frees) [] t
 where frees = freeVars t

reduceIds :: HsType -> HsType
reduceIds t = evalState (f t) (IntMap.empty, 0)
  where
    f :: HsType -> State (IntMap.IntMap TVarId, TVarId) HsType
    f (TypeVar i)           = TypeVar <$> g i
    f c@(TypeConstant _)    = return c
    f c@(TypeCons _)        = return c
    f (TypeArrow t1 t2)     = TypeArrow  <$> f t1 <*> f t2
    f (TypeApp   t1 t2)     = TypeApp    <$> f t1 <*> f t2
    f (TypeForall is cs t1) = TypeForall <$> mapM g is <*> mapM h cs <*> f t1
    g :: TVarId -> State (IntMap.IntMap TVarId, TVarId) TVarId
    g i = do
      (mapping, next) <- get
      case IntMap.lookup i mapping of
        Nothing -> do
          put (IntMap.insert i next mapping, next+1)
          return next
        Just x -> return x
    h (HsConstraint cls params) = HsConstraint cls <$> mapM f params

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

distinctify :: HsType -> HsType -> HsType
distinctify a b = let x = largestId a in incVarIds (+(x+1)) b

largestSubstsId :: Substs -> TVarId
largestSubstsId = IntMap.foldl' (\a b -> a `max` largestId b) 0

constraintMapTypes :: (HsType -> HsType) -> HsConstraint -> HsConstraint
constraintMapTypes f (HsConstraint a ts) = HsConstraint a (map f ts)


mkStaticClassEnv :: [HsTypeClass] -> [HsInstance] -> StaticClassEnv
mkStaticClassEnv tclasses insts = StaticClassEnv tclasses (helper insts)
  where
    helper :: [HsInstance] -> IntMap.IntMap [HsInstance]
    helper is = IntMap.fromListWith (++)
              $ [ (tclass_name $ instance_tclass i, [i]) | i <- is ]

constraintContainsVariables :: HsConstraint -> Bool
constraintContainsVariables = any ((-1/=).largestId) . constraint_params

-- TODO: it probably is a bad idea to have any unknown type class mapped to
--       this, as they might unify at some point.. even if they distinct.
unknownTypeClass :: (MonadMultiState QNameIndex m) => m HsTypeClass
unknownTypeClass = do
  qid <- getOrCreateQNameId $ QualifiedName [] "EXFUnknownTC"
  return $ HsTypeClass qid [] []

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

{-# LANGUAGE BangPatterns #-}

module Language.Haskell.Exference.Core.Internal.Unify
  ( unify
  , unifyOffset
  , unifyRight
  , unifyRightEqs
  , unifyRightOffset
  , TypeEq (..)
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe

import Debug.Hood.Observe
import Debug.Trace



data TypeEq = TypeEq !HsType
                     !HsType

data UniState1 = UniState1 [TypeEq] Substs
data UniState2 = UniState2 [TypeEq] Substs Substs

occursIn :: TVarId -> HsType -> Bool
occursIn i (TypeVar j)         = i==j
occursIn _ (TypeConstant _)    = False
occursIn _ (TypeCons _)        = False
occursIn i (TypeArrow t1 t2)   = occursIn i t1 || occursIn i t2
occursIn i (TypeApp t1 t2)     = occursIn i t1 || occursIn i t2
occursIn i (TypeForall js _ t) = not (i `elem` js) && occursIn i t

-- unification of types.
-- returns two substitutions: one for variables in the first type,
-- one for variables in the second. In the symmetric case,
-- substituting the right-hand (second) type will be preferred.
-- examples:
-- unify v C -> ([v=>C], [])
-- unify C v -> ([], [v=>C])
-- unify v w -> ([], [w=>v])
{-# INLINE unify #-}
unify :: HsType -> HsType -> Maybe (Substs, Substs)
unify ut1 ut2 = unify' $ UniState2 [TypeEq ut1 ut2] IntMap.empty IntMap.empty
  where
    unify' :: UniState2 -> Maybe (Substs, Substs)
    unify' (UniState2 [] l r)      = Just (l, r)
    unify' (UniState2 (x:xr) l r) = uniStep x >>= (
      \ms -> unify' $ case ms of
        Left (Left subst) -> let f = applySubst subst in UniState2
          [ TypeEq (f a) b | TypeEq a b <- xr ]
          (uncurry IntMap.insert subst $ IntMap.map f l)
          r
        Left (Right subst) -> let f = applySubst subst in UniState2
          [ TypeEq a (f b) | TypeEq a b <- xr ]
          l
          (uncurry IntMap.insert subst $ IntMap.map f r)
        Right eqs -> UniState2 (eqs++xr) l r
      )
    uniStep :: TypeEq -> Maybe (Either (Either Subst Subst) [TypeEq])
    uniStep (TypeEq (TypeVar i1) (TypeVar i2)) | i1==i2 = Just (Right [])
    uniStep (TypeEq (t1) (TypeVar i2)) = if occursIn i2 t1
      then Nothing
      else Just $ Left $ Right (i2, t1)
    uniStep (TypeEq (TypeVar i1) (t2)) = if occursIn i1 t2
      then Nothing
      else Just $ Left $ Left (i1, t2)
    uniStep (TypeEq (TypeConstant i1) (TypeConstant i2)) | i1==i2 = Just (Right [])
    uniStep (TypeEq (TypeCons s1) (TypeCons s2)) | s1==s2 = Just (Right [])
    uniStep (TypeEq (TypeArrow t1 t2) (TypeArrow t3 t4)) = Just (Right [TypeEq t1 t3, TypeEq t2 t4])
    uniStep (TypeEq (TypeApp t1 t2) (TypeApp t3 t4)) = Just (Right [TypeEq t1 t3, TypeEq t2 t4])
    -- TODO TypeForall unification
    -- THIS IS WRONG; WE IGNORE FORALLS FOR THE MOMENT
    uniStep (TypeEq (TypeForall _ _ t1) (t2)) = uniStep $ TypeEq t1 t2
    uniStep (TypeEq (t1) (TypeForall _ _ t2)) = uniStep $ TypeEq t1 t2
    uniStep _ = Nothing

{-# INLINE unifyOffset #-}                   -- left, rightOffset
unifyOffset :: HsType -> HsTypeOffset -> Maybe (Substs, Substs)
unifyOffset ut1 (HsTypeOffset ut2 offset) = unify' $ UniState2 [TypeEq ut1 ut2]
                                                               IntMap.empty
                                                               IntMap.empty
  where
    unify' :: UniState2 -> Maybe (Substs, Substs)
    unify' (UniState2 [] l r)      = Just (l, r)
    unify' (UniState2 (x:xr) l r) = uniStep x >>= (
      \ms -> unify' $ case ms of
        Left (Left subst) -> let f = applySubst subst in UniState2
          [ TypeEq (f a) b | TypeEq a b <- xr ]
          (uncurry IntMap.insert subst $ IntMap.map f l)
          r
        Left (Right (substInternal, substExtern)) ->
          let f = applySubst substInternal
          in UniState2
            [ TypeEq a (f b) | TypeEq a b <- xr ]
            l
            (uncurry IntMap.insert substExtern $ IntMap.map f r)
        Right eqs -> UniState2 (eqs++xr) l r
      )
    uniStep :: TypeEq -> Maybe (Either (Either Subst (Subst, Subst)) [TypeEq])
    uniStep (TypeEq (TypeVar i1) (TypeVar i2)) | i1==offset+i2 = Just (Right [])
    uniStep (TypeEq (t1) (TypeVar i2)) = if occursIn (offset+i2) t1
      then Nothing
      else Just $ Left $ Right ((i2, t1), (i2+offset, t1))
    uniStep (TypeEq (TypeVar i1) (t2)) = if occursIn (i1-offset) t2
      then Nothing
      else Just $ Left $ Left (i1, incVarIds (+offset) t2)
    uniStep (TypeEq (TypeConstant i1) (TypeConstant i2)) | i1==i2 = Just (Right [])
    uniStep (TypeEq (TypeCons s1) (TypeCons s2)) | s1==s2 = Just (Right [])
    uniStep (TypeEq (TypeArrow t1 t2) (TypeArrow t3 t4)) = Just (Right [TypeEq t1 t3, TypeEq t2 t4])
    uniStep (TypeEq (TypeApp t1 t2) (TypeApp t3 t4)) = Just (Right [TypeEq t1 t3, TypeEq t2 t4])
    -- TODO TypeForall unification
    -- THIS IS WRONG; WE IGNORE FORALLS FOR THE MOMENT
    uniStep (TypeEq (TypeForall _ _ t1) (t2)) = uniStep $ TypeEq t1 t2
    uniStep (TypeEq (t1) (TypeForall _ _ t2)) = uniStep $ TypeEq t1 t2
    uniStep _ = Nothing

-- treats the variables in the first parameter as constants, and returns
-- the variable bindings for the second parameter that unify both types.
{-# INLINE unifyRight #-}
unifyRight :: HsType -> HsType -> Maybe Substs
unifyRight ut1 ut2 = unify' $ UniState1 [TypeEq ut1 ut2] IntMap.empty
  where
    unify' :: UniState1 -> Maybe Substs
    unify' (UniState1 [] x) = Just x
    unify' (UniState1 (x:xr) ss) = uniStep x >>= (
      \r -> unify' $ case r of
        Left subst -> let f = applySubst subst in UniState1
          [ TypeEq a (f b) | TypeEq a b <- xr]
          (uncurry IntMap.insert subst $ IntMap.map f ss)
        Right eqs -> UniState1 (eqs++xr) ss
      )
    uniStep :: TypeEq -> Maybe (Either Subst [TypeEq])
    uniStep (TypeEq (TypeVar i1) (TypeVar i2)) | i1==i2 = Just (Right [])
    uniStep (TypeEq (t1) (TypeVar i2)) = if occursIn i2 t1
      then Nothing
      else Just (Left (i2, t1))
    uniStep (TypeEq (TypeVar _) _) = Nothing
    uniStep (TypeEq (TypeConstant i1) (TypeConstant i2)) | i1==i2 = Just (Right [])
    uniStep (TypeEq (TypeCons s1) (TypeCons s2)) | s1==s2 = Just (Right [])
    uniStep (TypeEq (TypeArrow t1 t2) (TypeArrow t3 t4)) = Just (Right [TypeEq t1 t3, TypeEq t2 t4])
    uniStep (TypeEq (TypeApp t1 t2) (TypeApp t3 t4)) = Just (Right [TypeEq t1 t3, TypeEq t2 t4])
    -- TODO TypeForall unification
    -- THIS IS WRONG; WE IGNORE FORALLS FOR THE MOMENT
    uniStep (TypeEq (TypeForall _ _ t1) t2)   = uniStep $ TypeEq t1 t2
    uniStep (TypeEq (t1) (TypeForall _ _ t2)) = uniStep $ TypeEq t1 t2
    uniStep _ = Nothing

-- treats the variables in the first parameter as constants, and returns
-- the variable bindings for the second parameter that unify both types.
{-# INLINE unifyRightOffset #-}
unifyRightOffset :: HsType -> HsTypeOffset -> Maybe Substs
unifyRightOffset ut1 (HsTypeOffset ut2 offset) = unify' $ UniState1 [TypeEq ut1 ut2] IntMap.empty
  where
    unify' :: UniState1 -> Maybe Substs
    unify' (UniState1 [] x) = Just x
    unify' (UniState1 (x:xr) ss) = uniStep x >>= (
      \r -> unify' $ case r of
        Left (substInternal, substExtern) ->
          let f = applySubst substInternal
          in UniState1
            [ TypeEq a (f b) | TypeEq a b <- xr]
            (uncurry IntMap.insert substExtern $ IntMap.map f ss)
        Right eqs -> UniState1 (eqs++xr) ss
      )
    uniStep :: TypeEq -> Maybe (Either (Subst, Subst) [TypeEq])
    uniStep (TypeEq (TypeVar i1) (TypeVar i2)) | i1==offset+i2 = Just (Right [])
    uniStep (TypeEq (t1) (TypeVar i2)) = if occursIn i2 t1
      then Nothing
      else Just (Left ((i2, t1), (i2+offset, t1)))
    uniStep (TypeEq (TypeVar _) _) = Nothing
    uniStep (TypeEq (TypeConstant i1) (TypeConstant i2)) | i1==i2 = Just (Right [])
    uniStep (TypeEq (TypeCons s1) (TypeCons s2)) | s1==s2 = Just (Right [])
    uniStep (TypeEq (TypeArrow t1 t2) (TypeArrow t3 t4)) = Just (Right [TypeEq t1 t3, TypeEq t2 t4])
    uniStep (TypeEq (TypeApp t1 t2) (TypeApp t3 t4)) = Just (Right [TypeEq t1 t3, TypeEq t2 t4])
    -- TODO TypeForall unification
    -- THIS IS WRONG; WE IGNORE FORALLS FOR THE MOMENT
    uniStep (TypeEq (TypeForall _ _ t1) t2)   = uniStep $ TypeEq t1 t2
    uniStep (TypeEq (t1) (TypeForall _ _ t2)) = uniStep $ TypeEq t1 t2
    uniStep _ = Nothing

{-# INLINE unifyRightEqs #-}
unifyRightEqs :: [TypeEq] -> Maybe Substs
unifyRightEqs teqs = unify' $ UniState1 teqs IntMap.empty
  where
    unify' :: UniState1 -> Maybe Substs
    unify' (UniState1 [] x) = Just x
    unify' (UniState1 (x:xr) ss) = uniStep x >>= (
      \r -> unify' $ case r of
        Left subst -> let f = applySubst subst in UniState1
          [ TypeEq a (f b) | TypeEq a b <- xr]
          (uncurry IntMap.insert subst $ IntMap.map f ss)
        Right eqs -> UniState1 (eqs++xr) ss
      )
    uniStep :: TypeEq -> Maybe (Either Subst [TypeEq])
    uniStep (TypeEq (TypeVar i1) (TypeVar i2)) | i1==i2 = Just (Right [])
    uniStep (TypeEq (t1) (TypeVar i2)) = if occursIn i2 t1
      then Nothing
      else Just (Left (i2, t1))
    uniStep (TypeEq (TypeVar _) _) = Nothing
    uniStep (TypeEq (TypeConstant i1) (TypeConstant i2)) | i1==i2 = Just (Right [])
    uniStep (TypeEq (TypeCons s1) (TypeCons s2)) | s1==s2 = Just (Right [])
    uniStep (TypeEq (TypeArrow t1 t2) (TypeArrow t3 t4)) = Just (Right [TypeEq t1 t3, TypeEq t2 t4])
    uniStep (TypeEq (TypeApp t1 t2) (TypeApp t3 t4)) = Just (Right [TypeEq t1 t3, TypeEq t2 t4])
    -- TODO TypeForall unification
    -- THIS IS WRONG; WE IGNORE FORALLS FOR THE MOMENT
    uniStep (TypeEq (TypeForall _ _ t1) t2)   = uniStep $ TypeEq t1 t2
    uniStep (TypeEq (t1) (TypeForall _ _ t2)) = uniStep $ TypeEq t1 t2
    uniStep _ = Nothing

--unifyDist :: HsType -> HsType -> Maybe Substs
--unifyDist t1 t2 = unify t1 $ distinctify t1 t2

-- tries to unify two types, under the assumption that one is supposed
-- to serve as the parameter (at some levels) to the other one (when at
-- least one is a function, that is.)
-- either is BROKEN, or does something other than i expected.
-- not used (or needed) anyway, so.. deletion candidate.
{-
inflateUnify :: HsType -> HsType -> [HsType]
inflateUnify t1 t2 =
  let d1 = arrowDepth t1
      d2 = arrowDepth t2
  in if d1 > d2
    then f t1 $ inflateTo d1 t2
    else f t2 $ inflateTo d2 t1
  where
    inflateTo :: Int -> HsType -> [HsType]
    inflateTo n t = map (createArrow t (map TypeVar [1000..998+n])) [0..n-1]
    createArrow t varIds i =
      let (l,r) = splitAt i varIds
          types = l++[t]++r
      in foldr1 TypeArrow types
    f :: HsType -> [HsType] -> [HsType]
    f ft1 ft2s = catMaybes [g ft1 (distinctify ft1 ft2) | ft2 <- ft2s]
    g :: HsType -> HsType -> Maybe HsType
    g gt1 gt2 = fmap (\subst -> reduceIds $ applySubsts subst gt1) (unify gt1 gt2)
-}

{-# LANGUAGE RankNTypes #-}

module Match where



import Type
import Unify
import qualified Data.Map as M

import Debug.Hood.Observe
import Debug.Trace




applyN :: Int -> HsType -> HsType -> Maybe HsType
applyN nParam pType fType =
  let (p, rest) = splitF nParam fType
  in (\substs -> reduceIds $ applySubsts substs rest) `fmap` unify p (distinctify p pType)
  where
    splitF :: Int -> HsType -> (HsType, HsType)
    splitF n t =
      let f :: Int
            -> HsType
            -> (HsType -> HsType)
            -> (HsType, HsType)
          f 0 (TypeArrow t1 t2) left = (t1, left t2)
          f m (TypeArrow t1 t2) left = f (m-1) t2 (TypeArrow (left t1))
       in f n t id

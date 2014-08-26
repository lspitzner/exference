module Expression
  ( Expression (..)
  , fillExprHole
  , simplifyLets
  )
where



import Type
import Data.List ( intercalate )
import Data.Function ( on )

import Debug.Hood.Observe



data Expression = ExpVar TVarId
                | ExpLit String
                | ExpLambda TVarId Expression
                | ExpApply Expression Expression
                | ExpHole TVarId
                | ExpLetMatch String [TVarId] Expression Expression
                | ExpLet TVarId Expression Expression
  deriving Eq

-- $( derive makeNFData ''Expression )

instance Show Expression where
  showsPrec _ (ExpVar i) = showString $ showVar i
  showsPrec _ (ExpLit s) = showString s
  showsPrec d (ExpLambda i e) =
    showParen (d>0) $ showString ("\\" ++ showVar i ++ " -> ") . showsPrec 1 e
  showsPrec d (ExpApply e1 e2) =
    showParen (d>1) $ showsPrec 2 e1 . showString " " . showsPrec 3 e2
  showsPrec _ (ExpHole i) = showString $ "_" ++ showVar i
  showsPrec d (ExpLetMatch n vars bindExp inExp) =
      showParen (d>2)
    $ showString ("let ("++n++" "++intercalate " " (map showVar vars) ++ ") = ")
    . shows bindExp . showString " in " . showsPrec 0 inExp
  showsPrec d (ExpLet i bindExp inExp) =
      showParen (d>2)
    $ showString ("let " ++ showVar i ++ " = ")
    . showsPrec 3 bindExp
    . showString " in "
    . showsPrec 0 inExp

instance Observable Expression where
  observer x = observeOpaque (show x) x
fillExprHole :: TVarId -> Expression -> Expression -> Expression
fillExprHole vid t orig@(ExpHole j) | vid==j = t
                                    | otherwise = orig
fillExprHole vid t (ExpLambda i ty) = ExpLambda i $ fillExprHole vid t ty
fillExprHole vid t (ExpApply e1 e2) = ExpApply (fillExprHole vid t e1)
                                               (fillExprHole vid t e2)
fillExprHole vid t (ExpLetMatch n vars bindExp inExp) =
  ExpLetMatch n vars (fillExprHole vid t bindExp) (fillExprHole vid t inExp)
fillExprHole vid t (ExpLet i bindExp inExp) =
  ExpLet i (fillExprHole vid t bindExp) (fillExprHole vid t inExp)
fillExprHole _ _ t@(ExpLit _) = t
fillExprHole _ _ t@(ExpVar _) = t

replaceVar :: TVarId -> Expression -> Expression -> Expression
replaceVar vid t orig@(ExpVar j) | vid==j = t
                                   | otherwise = orig
replaceVar vid t (ExpLambda i ty) = ExpLambda i $ replaceVar vid t ty
replaceVar vid t (ExpApply e1 e2) = ExpApply (replaceVar vid t e1)
                                               (replaceVar vid t e2)
replaceVar vid t (ExpLetMatch n vars bindExp inExp) =
  ExpLetMatch n vars (replaceVar vid t bindExp) (replaceVar vid t inExp)
replaceVar vid t (ExpLet i bindExp inExp) =
  ExpLet i (replaceVar vid t bindExp) (replaceVar vid t inExp)
replaceVar _ _ t@(ExpLit _) = t
replaceVar _ _ t@(ExpHole _) = t

simplifyLets :: Expression -> Expression
simplifyLets e@(ExpVar _) = e
simplifyLets e@(ExpLit _) = e
simplifyLets (ExpLambda i e) = ExpLambda i $ simplifyLets e
simplifyLets (ExpApply e1 e2) = ExpApply (simplifyLets e1) (simplifyLets e2)
simplifyLets e@(ExpHole _) = e
simplifyLets e@(ExpLetMatch name vids bindExp inExp) =
  ExpLetMatch name vids (simplifyLets bindExp) (simplifyLets inExp)
simplifyLets e@(ExpLet i bindExp inExp) = case countUses i inExp of
  0 -> simplifyLets inExp
  1 -> simplifyLets $ replaceVar i bindExp inExp
  _ -> e

countUses :: TVarId -> Expression -> Int
countUses i (ExpVar j) | i==j = 1
                       | otherwise = 0
countUses i (ExpLit _) = 0
countUses i (ExpLambda j e) | i==j = 0
                            | otherwise = countUses i e
countUses i (ExpApply e1 e2) = ((+) `on` countUses i) e1 e2
countUses i (ExpHole _) = 0
countUses i (ExpLetMatch _ _ bindExp inExp) = ((+) `on` countUses i) bindExp inExp
countUses i (ExpLet _ bindExp inExp) = ((+) `on` countUses i) bindExp inExp

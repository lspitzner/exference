module Expression
  ( Expression (..)
  , fillExprHole
  )
where



import Type
import Data.List ( intercalate )


data Expression = ExpVar TVarId
                | ExpLit String
                | ExpLambda TVarId Expression
                | ExpApply Expression Expression
                | ExpHole TVarId
                | ExpLetMatch String [TVarId] Expression Expression
                | ExpLet TVarId Expression Expression

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

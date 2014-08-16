module Expression where



import Type



data Expression = ExpVar TVarId
                | ExpLit String
                | ExpLambda TVarId Expression
                | ExpApply Expression Expression
                | ExpHole TVarId

-- $( derive makeNFData ''Expression )

instance Show Expression where
  showsPrec _ (ExpVar i) = showString $ showVar i
  showsPrec _ (ExpLit s) = showString s
  showsPrec d (ExpLambda i e) =
    showParen (d>0) $ showString ("\\" ++ showVar i ++ " -> ") . showsPrec 1 e
  showsPrec d (ExpApply e1 e2) =
    showParen (d>1) $ showsPrec 2 e1 . showString " " . showsPrec 2 e2
  showsPrec _ (ExpHole i) = showString $ "_" ++ showVar i

fillExprHole :: TVarId -> Expression -> Expression -> Expression
fillExprHole vid t (ExpHole j) | vid==j = t
fillExprHole vid t (ExpLambda i ty) = ExpLambda i $ fillExprHole vid t ty
fillExprHole vid t (ExpApply e1 e2) = ExpApply (fillExprHole vid t e1)
                                              (fillExprHole vid t e2)
fillExprHole _ _ t = t

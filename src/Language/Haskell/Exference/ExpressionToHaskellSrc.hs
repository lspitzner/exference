{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.ExpressionToHaskellSrc
  ( convert
  )
where



import qualified Language.Haskell.Exference.Expression as E
import qualified Language.Haskell.Exference.Type as T
import Language.Haskell.Exts.Syntax



convert :: E.Expression -> Decl
convert e = h e []
  where
    h (E.ExpLambda i e1) is = h e1 (i:is)
    h rhsExp is =
        FunBind [Match noLoc (Ident "f") params Nothing rhs (BDecls[])]
      where
        params = map (PVar . Ident . T.showVar) $ reverse is
        rhs = UnGuardedRhs $ convertExp rhsExp

convertExp :: E.Expression -> Exp
convertExp (E.ExpVar i) = Var $ UnQual $ Ident $ T.showVar i
convertExp (E.ExpLit c) = Con $ UnQual $ Ident $ c
convertExp (E.ExpLambda i e) =
    Lambda noLoc [PVar $ Ident $ T.showVar i] (convertExp e)
convertExp (E.ExpApply e1 e2)
  | (E.ExpApply op e0) <- e1,
    (E.ExpLit ('(':opId1)) <- op,
    opId <- takeWhile (/=')') opId1 =
      if opId == ","
        then Tuple Unboxed [convertExp e0, convertExp e2]
        else InfixApp (convertExp e0)
                        (QVarOp $ UnQual $ Symbol $ opId)
                        (convertExp e2)
  | otherwise = App (convertExp e1) (Paren $ convertExp e2)
convertExp (E.ExpHole i) = Var $ UnQual $ Ident $ "_"++T.showVar i
convertExp (E.ExpLet i bindE inE) =
  let convBind = PatBind noLoc
                   (PVar $ Ident $ T.showVar i)
                   (UnGuardedRhs $ convertExp bindE)
                   (BDecls [])
  in mergeLet convBind (convertExp inE)
convertExp (E.ExpLetMatch n ids bindE inE) =
  let convBind = PatBind noLoc
                   (PParen $ PApp (UnQual $ Ident $ n)
                                      (map (PVar . Ident . T.showVar)
                                           ids))
                   (UnGuardedRhs $ convertExp bindE)
                   (BDecls [])
  in mergeLet convBind (convertExp inE)

mergeLet :: Decl -> Exp -> Exp
mergeLet convBind (Let (BDecls otherBinds) finalIn)
  = Let (BDecls $ convBind:otherBinds) finalIn
mergeLet convBind finalIn                 
  = Let (BDecls [convBind]) finalIn

noLoc :: SrcLoc
noLoc = SrcLoc "" 0 0

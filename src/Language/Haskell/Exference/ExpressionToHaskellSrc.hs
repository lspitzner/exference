{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.ExpressionToHaskellSrc
  ( convert
  )
where



import qualified Language.Haskell.Exference.Expression as E
import qualified Language.Haskell.Exference.Type as T
import Language.Haskell.Exts.Syntax



-- TODO:
-- 1) merge nested lambdas

convert :: E.Expression -> Exp
convert e = h e []
  where
    h (E.ExpLambda i e1) is = h e1 (i:is)
    h rhsExp [] = convertExp rhsExp
    h rhsExp is =
        Lambda noLoc params (convertExp rhsExp)
        -- FunBind [Match noLoc (Ident "f") params Nothing rhs (BDecls[])]
      where
        params = map (PVar . Ident . T.showVar) $ reverse is
        -- rhs = UnGuardedRhs $ convertExp rhsExp

convertExp :: E.Expression -> Exp
convertExp e = convertInternal 0 e

parens :: Bool -> Exp -> Exp
parens True e = Paren e
parens False e = e

convertInternal :: Int -> E.Expression -> Exp
convertInternal _ (E.ExpVar i) = Var $ UnQual $ Ident $ T.showVar i
convertInternal _ (E.ExpLit c) = Con $ UnQual $ Ident $ c
convertInternal p (E.ExpLambda i e) = parens (p>=1) $
    Lambda noLoc [PVar $ Ident $ T.showVar i] (convertInternal 0 e)
convertInternal p (E.ExpApply e1 e2)
  | (E.ExpApply op e0) <- e1,
    (E.ExpLit ('(':opId1)) <- op,
    opId <- takeWhile (/=')') opId1 =
      if opId == ","
        then Tuple Boxed [convertInternal 0 e0, convertInternal 0 e2]
        else parens (p>=2) $ InfixApp (convertInternal 1 e0)
                        (QVarOp . UnQual . Symbol $ opId)
                        (convertInternal 2 e2)
  | otherwise = parens (p>=3)
              $ App (convertInternal 2 e1) (convertInternal 3 e2)
convertInternal _ (E.ExpHole i) = Var $ UnQual $ Ident $ "_"++T.showVar i
convertInternal p (E.ExpLet i bindE inE) =
  let convBind = PatBind noLoc
                   (PVar $ Ident $ T.showVar i)
                   (UnGuardedRhs $ convertInternal 0 bindE)
                   (BDecls [])
  in parens (p>=2) $ mergeLet convBind (convertInternal 0 inE)
convertInternal p (E.ExpLetMatch n ids bindE inE) =
  let convBind = PatBind noLoc
                   (PParen $ PApp (UnQual $ Ident $ n)
                                      (map (PVar . Ident . T.showVar)
                                           ids))
                   (UnGuardedRhs $ convertInternal 0 bindE)
                   (BDecls [])
  in parens (p>=2) $ mergeLet convBind (convertInternal 0 inE)

mergeLet :: Decl -> Exp -> Exp
mergeLet convBind (Let (BDecls otherBinds) finalIn)
  = Let (BDecls $ convBind:otherBinds) finalIn
mergeLet convBind finalIn                 
  = Let (BDecls [convBind]) finalIn

noLoc :: SrcLoc
noLoc = SrcLoc "" 0 0

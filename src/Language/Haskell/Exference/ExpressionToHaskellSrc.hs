{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.ExpressionToHaskellSrc
  ( convert
  , convertToFunc
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
      where
        params = map (PVar . Ident . T.showVar) $ reverse is

convertToFunc :: String -> E.Expression -> Decl
convertToFunc ident e = h e []
  where
    h (E.ExpLambda i e1) is = h e1 (i:is)
    h rhsExp is =
        FunBind [Match noLoc (Ident ident) params Nothing rhs (BDecls[])]
      where
        params = map (PVar . Ident . T.showVar) $ reverse is
        rhs = UnGuardedRhs $ convertExp rhsExp

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
convertInternal p (E.ExpApply e1 pe) = recurseApply e1 [pe]
  where
    recurseApply e pes
      |   (E.ExpApply e1' pe')     <- e
        = recurseApply e1' (pe':pes)
      |   (E.ExpLit ('(':',':opR)) <- e
      ,   length opR+1==length pes
        = Tuple Boxed $ map (convertInternal 0) pes
      |   (E.ExpLit ('(':opR))     <- e
      ,   [p1,p2] <- pes
        = parens (p>=2)
        $ InfixApp (convertInternal 1 p1)
                   (QVarOp $ UnQual $ Symbol $ takeWhile (/=')') opR)
                   (convertInternal 2 p2)
      |   otherwise
        = parens (p>=3)
        $ foldl App
                (convertInternal 2 e)
                (map (convertInternal 3) pes)
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
convertInternal p (E.ExpCaseMatch bindE alts) =
  let e = convertInternal 0 bindE
      as = [ Alt noLoc
                 (PApp (UnQual $ Ident $ c)
                       (map (PVar . Ident . T.showVar) vars))
                 (UnGuardedRhs $ convertInternal 0 expr)
                 (BDecls [])
           | (c, vars, expr) <- alts
           ]
  in parens (p>=2) $ Case e as

mergeLet :: Decl -> Exp -> Exp
mergeLet convBind (Let (BDecls otherBinds) finalIn)
  = Let (BDecls $ convBind:otherBinds) finalIn
mergeLet convBind finalIn                 
  = Let (BDecls [convBind]) finalIn

noLoc :: SrcLoc
noLoc = SrcLoc "" 0 0

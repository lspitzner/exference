{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.ExpressionToHaskellSrc
  ( convert
  , convertToFunc
  )
where



import qualified Language.Haskell.Exference.Core.Expression as E
import qualified Language.Haskell.Exference.Core.Types as T
import Language.Haskell.Exts.Syntax



-- TODO:
-- 1) merge nested lambdas

-- qualification level -> internal-expression -> haskell-src-expression
-- level 0 = no qualication
-- level 1 = qualification for anything but infix operators
-- level 2 = full qualification (prevents infix operators)
convert :: Int -> E.Expression -> Exp
convert q e = h e []
  where
    h (E.ExpLambda i e1) is = h e1 (i:is)
    h rhsExp [] = convertExp q rhsExp
    h rhsExp is =
        Lambda noLoc params (convertExp q rhsExp)
      where
        params = map (PVar . Ident . T.showVar) $ reverse is

convertToFunc :: Int -> String -> E.Expression -> Decl
convertToFunc q ident e = h e []
  where
    h (E.ExpLambda i e1) is = h e1 (i:is)
    h rhsExp is =
        FunBind [Match noLoc (Ident ident) params Nothing rhs (BDecls[])]
      where
        params = map (PVar . Ident . T.showVar) $ reverse is
        rhs = UnGuardedRhs $ convertExp q rhsExp

-- qualification level -> internal-expression -> haskell-src-expression
-- level 0 = no qualication
-- level 1 = qualification for anything but infix operators
-- level 2 = full qualification (prevents infix operators)
convertExp :: Int -> E.Expression -> Exp
convertExp q e = convertInternal q 0 e

parens :: Bool -> Exp -> Exp
parens True e = Paren e
parens False e = e

-- qualification level -> precedence -> expression
-- level 0 = no qualication
-- level 1 = qualification for anything but infix operators
-- level 2 = full qualification (prevents infix operators)
convertInternal :: Int -> Int -> E.Expression -> Exp
convertInternal _ _ (E.ExpVar i) = Var $ UnQual $ Ident $ T.showVar i
convertInternal q _ (E.ExpName qn) = Con $ UnQual $ Ident $ convertName q qn
convertInternal q p (E.ExpLambda i e) = parens (p>=1) $
    Lambda noLoc [PVar $ Ident $ T.showVar i] (convertInternal q 0 e)
convertInternal q p (E.ExpApply e1 pe) = recurseApply e1 [pe]
  where
    recurseApply e pes
      | (E.ExpApply e1' pe')     <- e
          = recurseApply e1' (pe':pes)
      | q<2
      , (E.ExpName (T.TupleCon i)) <- e
      , i==length pes
          = Tuple Boxed $ map (convertInternal q 0) pes
      | q<2
      , (E.ExpName (T.QualifiedName _ ('(':opR))) <- e
      , [p1,p2] <- pes
          = parens (p>=2)
          $ InfixApp (convertInternal q 1 p1)
                     (QVarOp $ UnQual $ Symbol $ takeWhile (/=')') opR)
                     (convertInternal q 2 p2)
      | otherwise
          = parens (p>=3)
          $ foldl App
                  (convertInternal q 2 e)
                  (map (convertInternal q 3) pes)
convertInternal _ _ (E.ExpHole i) = Var $ UnQual $ Ident $ "_"++T.showVar i
convertInternal q p (E.ExpLet i bindE inE) =
  let convBind = PatBind noLoc
                   (PVar $ Ident $ T.showVar i)
                   (UnGuardedRhs $ convertInternal q 0 bindE)
                   (BDecls [])
  in parens (p>=2) $ mergeLet convBind (convertInternal q 0 inE)
convertInternal q p (E.ExpLetMatch n ids bindE inE) =
  let convBind = PatBind noLoc
                   (PParen $ PApp (UnQual $ Ident $ convertName q n)
                                      (map (PVar . Ident . T.showVar)
                                           ids))
                   (UnGuardedRhs $ convertInternal q 0 bindE)
                   (BDecls [])
  in parens (p>=2) $ mergeLet convBind (convertInternal q 0 inE)
convertInternal q p (E.ExpCaseMatch bindE alts) =
  let e = convertInternal q 0 bindE
      as = [ Alt noLoc
                 (PApp (UnQual $ Ident $ convertName q c)
                       (map (PVar . Ident . T.showVar) vars))
                 (UnGuardedRhs $ convertInternal q 0 expr)
                 (BDecls [])
           | (c, vars, expr) <- alts
           ]
  in parens (p>=2) $ Case e as

convertName :: Int -> T.QualifiedName -> String
convertName 0 (T.QualifiedName _ n) = n
convertName _ qn = show qn

mergeLet :: Decl -> Exp -> Exp
mergeLet convBind (Let (BDecls otherBinds) finalIn)
  = Let (BDecls $ convBind:otherBinds) finalIn
mergeLet convBind finalIn                 
  = Let (BDecls [convBind]) finalIn

noLoc :: SrcLoc
noLoc = SrcLoc "" 0 0

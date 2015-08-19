{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Exference.ExpressionToHaskellSrc
  ( convert
  , convertToFunc
  )
where



import qualified Language.Haskell.Exference.Core.Expression as E
import qualified Language.Haskell.Exference.Core.Types as T
import qualified Language.Haskell.Exference.Core.TypeUtils as TU
import Language.Haskell.Exts.Syntax

import Control.Monad.Trans.MultiRWS

import Control.Monad ( forM )

import Control.Applicative



-- TODO:
-- 1) merge nested lambdas

-- qualification level -> internal-expression -> haskell-src-expression
-- level 0 = no qualication
-- level 1 = qualification for anything but infix operators
-- level 2 = full qualification (prevents infix operators)
convert :: ( MonadMultiState T.QNameIndex m
           , Functor m
           )
        => Int
        -> E.Expression
        -> m Exp
convert q e = h e []
  where
    h (E.ExpLambda i e1) is = h e1 (i:is)
    h rhsExp [] = convertExp q rhsExp
    h rhsExp is =
        Lambda noLoc params <$> convertExp q rhsExp
      where
        params = map (PVar . Ident . T.showVar) $ reverse is

convertToFunc :: ( MonadMultiState T.QNameIndex m
                 , Functor m
                 )
              => Int
              -> String
              -> E.Expression
              -> m Decl
convertToFunc q ident e = h e []
  where
    h (E.ExpLambda i e1) is = h e1 (i:is)
    h rhsExp is = do
      rhs' <- UnGuardedRhs <$> convertExp q rhsExp
      let params = map (PVar . Ident . T.showVar) $ reverse is
      return $ FunBind [Match noLoc (Ident ident) params Nothing rhs' (BDecls[])]

-- qualification level -> internal-expression -> haskell-src-expression
-- level 0 = no qualication
-- level 1 = qualification for anything but infix operators
-- level 2 = full qualification (prevents infix operators)
convertExp :: ( MonadMultiState T.QNameIndex m 
              , Functor m
              )
           => Int
           -> E.Expression
           -> m Exp
convertExp q e = convertInternal q 0 e

parens :: Bool -> Exp -> Exp
parens True e = Paren e
parens False e = e

-- qualification level -> precedence -> expression
-- level 0 = no qualication
-- level 1 = qualification for anything but infix operators
-- level 2 = full qualification (prevents infix operators)
convertInternal :: forall m
                 . ( MonadMultiState T.QNameIndex m
                   , Functor m
                   )
                => Int
                -> Int
                -> E.Expression
                -> m Exp
convertInternal _ _ (E.ExpVar i) = return $ Var
                                          $ UnQual
                                          $ Ident
                                          $ T.showVar i
convertInternal q _ (E.ExpName qn) = Con . UnQual . Ident
                                     <$> convertName q qn
convertInternal q p (E.ExpLambda i e) =
  parens (p>=1) . (Lambda noLoc [PVar $ Ident $ T.showVar i])
  <$> convertInternal q 0 e
convertInternal q p (E.ExpApply e1 pe) = recurseApply e1 [pe]
  where
    defaultApply :: E.Expression -> [E.Expression] -> m Exp
    defaultApply e pes = do
      f <- convertInternal q 2 e
      ps <- mapM (convertInternal q 3) pes
      return $ parens (p>=3) $ foldl App f ps
    recurseApply :: E.Expression -> [E.Expression] -> m Exp
    recurseApply (E.ExpApply e1' pe') pes = recurseApply e1' (pe':pes)
    recurseApply e@(E.ExpName qnid) pes = do
      qname <- TU.lookupQNameId qnid
      case qname of
        Just (T.TupleCon i)
          | i==length pes
          , q<2 ->
            Tuple Boxed <$> mapM (convertInternal q 0) pes
        Just T.Cons
          | q<2
          , [p1, p2] <- pes -> do
              q1 <- convertInternal q 1 p1
              q2 <- convertInternal q 2 p2
              return $ parens (p>=2) $ InfixApp
                q1
                (QVarOp $ UnQual $ Symbol ":")
                q2            
        Just (T.QualifiedName _ ('(':opR))
          | q<2
          , [p1, p2] <- pes -> do
              q1 <- convertInternal q 1 p1
              q2 <- convertInternal q 2 p2
              return $ parens (p>=2) $ InfixApp
                q1
                (QVarOp $ UnQual $ Symbol $ takeWhile (/=')') opR)
                q2
        _ -> defaultApply e pes
    recurseApply e pes = defaultApply e pes
convertInternal _ _ (E.ExpHole i) = return $ Var
                                           $ UnQual
                                           $ Ident
                                           $ "_"++T.showVar i
convertInternal q p (E.ExpLet i bindE inE) = do
  rhs <- convertInternal q 0 bindE
  let convBind = PatBind noLoc
                   (PVar $ Ident $ T.showVar i)
                   (UnGuardedRhs $ rhs)
                   (BDecls [])
  e <- convertInternal q 0 inE
  return $ parens (p>=2) $ mergeLet convBind e
convertInternal q p (E.ExpLetMatch n ids bindE inE) = do
  rhs <- convertInternal q 0 bindE
  name <- convertName q n
  let convBind = PatBind noLoc
                   (PParen $ PApp (UnQual $ Ident $ name)
                                      (map (PVar . Ident . T.showVar)
                                           ids))
                   (UnGuardedRhs $ rhs)
                   (BDecls [])
  e <- convertInternal q 0 inE
  return $ parens (p>=2) $ mergeLet convBind e
convertInternal q p (E.ExpCaseMatch bindE alts) = do
  e <- convertInternal q 0 bindE
  as <- alts `forM` \(c, vars, expr) -> do
    rhs <- convertInternal q 0 expr
    name <- convertName q c
    return $ Alt noLoc
        (PApp (UnQual $ Ident $ name)
              (map (PVar . Ident . T.showVar) vars))
        (UnGuardedRhs $ rhs)
        (BDecls [])
  return $ parens (p>=2) $ Case e as

convertName :: MonadMultiState T.QNameIndex m => Int -> T.QNameId -> m String
convertName d qnid = do
  qn <- TU.lookupQNameId qnid
  return $ case (d, qn) of
    (0, Just (T.QualifiedName _ n)) -> n
    (_, Just n)                     -> show n
    (_, Nothing)                    -> "BADQNAMEID"

mergeLet :: Decl -> Exp -> Exp
mergeLet convBind (Let (BDecls otherBinds) finalIn)
  = Let (BDecls $ convBind:otherBinds) finalIn
mergeLet convBind finalIn                 
  = Let (BDecls [convBind]) finalIn

noLoc :: SrcLoc
noLoc = SrcLoc "" 0 0

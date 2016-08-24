{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Language.Haskell.Exference.ExpressionToHaskellSrc
  ( convert
  , convertToFunc
  )
where



import qualified Language.Haskell.Exference.Core.Expression as E
import qualified Language.Haskell.Exference.Core.Types as T
import qualified Language.Haskell.Exference.Core.TypeUtils as TU
import Language.Haskell.Exts.Syntax

import Control.Monad ( forM )

import Control.Applicative

import qualified Data.Map as M
import Data.Map ( Map )

import Data.HList.ContainsType

import Data.Functor.Identity

import Control.Monad.Trans.MultiState



-- TODO:
-- 1) merge nested lambdas

-- qualification level -> internal-expression -> haskell-src-expression
-- level 0 = no qualication
-- level 1 = qualification for anything but infix operators
-- level 2 = full qualification (prevents infix operators)
convert :: Int
        -> E.Expression
        -> Exp
convert q e = runIdentity
            $ runMultiStateTNil
            $ withMultiStateA (M.empty :: Map T.TVarId T.HsType)
            $ do
                E.collectVarTypes e
                h e []
  where
    h (E.ExpLambda i ty e1) is = h e1 ((i, ty):is)
    h rhsExp [] = convertExp q rhsExp
    h rhsExp is = [ Lambda noLoc (map (PVar . Ident) params) cr
                  | cr <- convertExp q rhsExp
                  , params <- mapM (T.showTypedVar . fst)
                                   (reverse is)
                  ]

convertToFunc :: Int
              -> String
              -> E.Expression
              -> Decl
convertToFunc q ident e = runIdentity
                        $ runMultiStateTNil
                        $ withMultiStateA (M.empty :: Map T.TVarId T.HsType)
                        $ do
                            E.collectVarTypes e
                            h e []
  where
    h (E.ExpLambda i ty e1) is = h e1 ((i, ty):is)
    h rhsExp is = [ FunBind [Match noLoc
                                   (Ident ident)
                                   (map (PVar . Ident) params)
                                   Nothing
                                   rhs'
                                   Nothing]
                  | rhs' <- UnGuardedRhs <$> convertExp q rhsExp
                  , params <- mapM (T.showTypedVar . fst) (reverse is)
                  ]

-- qualification level -> internal-expression -> haskell-src-expression
-- level 0 = no qualication
-- level 1 = qualification for anything but infix operators
-- level 2 = full qualification (prevents infix operators)
convertExp :: Int -> E.Expression -> MultiState '[Map T.TVarId T.HsType] Exp
convertExp q = convertInternal q 0

parens :: Bool -> Exp -> Exp
parens True e = Paren e
parens False e = e

-- qualification level -> precedence -> expression
-- level 0 = no qualication
-- level 1 = qualification for anything but infix operators
-- level 2 = full qualification (prevents infix operators)
convertInternal
  :: Int -> Int -> E.Expression -> MultiState '[Map T.TVarId T.HsType] Exp
convertInternal _ _ (E.ExpVar i _) = Var . UnQual . Ident
                                    <$> T.showTypedVar i
convertInternal q _ (E.ExpName qn) =
  return $ Con $ UnQual $ Ident $ convertName q qn
convertInternal q p (E.ExpLambda i _ e) =
  [ parens (p>=1) $ Lambda noLoc [PVar $ Ident $ vname] ce
  | ce <- convertInternal q 0 e
  , vname <- T.showTypedVar i
  ]
convertInternal q p (E.ExpApply e1 pe) = recurseApply e1 [pe]
  where
    defaultApply :: E.Expression -> [E.Expression] -> MultiState '[Map T.TVarId T.HsType] Exp
    defaultApply e pes = do
      f  <- convertInternal q 2 e
      ps <- mapM (convertInternal q 3) pes
      return $ parens (p>=3) $ foldl App f ps
    recurseApply :: E.Expression -> [E.Expression] -> MultiState '[Map T.TVarId T.HsType] Exp
    recurseApply (E.ExpApply e1' pe') pes = recurseApply e1' (pe':pes)
    recurseApply e@(E.ExpName qname) pes = do
      case qname of
        T.TupleCon i
          | i==length pes
          , q<2 ->
            Tuple Boxed <$> mapM (convertInternal q 0) pes
        T.Cons
          | q<2
          , [p1, p2] <- pes -> do
              q1 <- convertInternal q 1 p1
              q2 <- convertInternal q 2 p2
              return $ parens (p>=2) $ InfixApp
                q1
                (QVarOp $ UnQual $ Symbol ":")
                q2            
        T.QualifiedName _ ('(':opR)
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
convertInternal q p (E.ExpLet i _ bindE inE) = do
  rhs <- convertInternal q 0 bindE
  varName <- T.showTypedVar i
  let convBind = PatBind noLoc
                   (PVar $ Ident $ varName)
                   (UnGuardedRhs $ rhs)
                   Nothing
  e <- convertInternal q 0 inE
  return $ parens (p>=2) $ mergeLet convBind e
convertInternal q p (E.ExpLetMatch n ids bindE inE) = do
  rhs <- convertInternal q 0 bindE
  let name = convertName q n
  varNames <- mapM (T.showTypedVar . fst) ids
  let convBind = PatBind noLoc
                   (PParen $ PApp (UnQual $ Ident $ name)
                                  (map (PVar . Ident) varNames))
                   (UnGuardedRhs $ rhs)
                   Nothing
  e <- convertInternal q 0 inE
  return $ parens (p>=2) $ mergeLet convBind e
convertInternal q p (E.ExpCaseMatch bindE alts) = do
  e <- convertInternal q 0 bindE
  as <- alts `forM` \(c, vars, expr) -> do
    rhs <- convertInternal q 0 expr
    let name = convertName q c
    varNames <- mapM (T.showTypedVar . fst) vars
    return $ Alt noLoc
        (PApp (UnQual $ Ident $ name)
              (map (PVar . Ident) varNames))
        (UnGuardedRhs $ rhs)
        Nothing
  return $ parens (p>=2) $ Case e as

convertName :: Int -> T.QualifiedName -> String
convertName d qn = case (d, qn) of
  (0, T.QualifiedName _ n) -> n
  (_, n)                   -> show n

mergeLet :: Decl -> Exp -> Exp
mergeLet convBind (Let (BDecls otherBinds) finalIn)
  = Let (BDecls $ convBind:otherBinds) finalIn
mergeLet convBind finalIn                 
  = Let (BDecls [convBind]) finalIn

noLoc :: SrcLoc
noLoc = SrcLoc "" 0 0

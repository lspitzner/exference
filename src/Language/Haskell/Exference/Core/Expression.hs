{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Language.Haskell.Exference.Core.Expression
  ( Expression (..)
  , showExpression
  , showExpressionPure
  , fillExprHole
  , collectVarTypes
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Data.List ( intercalate )
import Data.Function ( on )
import Data.Maybe ( fromMaybe )
import Control.Monad ( forM, forM_, liftM )
import Data.Functor.Identity ( runIdentity )
import Data.Functor ( (<$>) )

import Control.DeepSeq.Generics
import GHC.Generics

import Control.Monad.Trans.MultiRWS
import Data.HList.ContainsType

import Debug.Hood.Observe
import Data.Map ( Map )
import qualified Data.Map as M



data Expression = ExpVar TVarId HsType -- a
                   -- (type is just for choosing better id when printing)
                | ExpName QNameId -- Prelude.zip
                | ExpLambda TVarId HsType Expression -- \x -> exp
                | ExpApply Expression Expression -- f x
                | ExpHole TVarId                 -- h
                | ExpLetMatch QNameId [(TVarId, HsType)] Expression Expression
                            -- let (Foo a b c) = bExp in inExp
                | ExpLet TVarId HsType Expression Expression
                            -- let x = bExp in inExp
                | ExpCaseMatch
                    Expression
                    [(QNameId, [(TVarId, HsType)], Expression)]
                     -- case mExp of Foo a b -> e1; Bar c d -> e2
  deriving (Eq, Generic)

instance NFData Expression where rnf = genericRnf

-- instance Show Expression where
--   showsPrec _ (ExpVar i) = showString $ showVar i
--   showsPrec d (ExpName s) = showsPrec d s
--   showsPrec d (ExpLambda i e) =
--     showParen (d>0) $ showString ("\\" ++ showVar i ++ " -> ") . showsPrec 1 e
--   showsPrec d (ExpApply e1 e2) =
--     showParen (d>1) $ showsPrec 2 e1 . showString " " . showsPrec 3 e2
--   showsPrec _ (ExpHole i) = showString $ "_" ++ showVar i
--   showsPrec d (ExpLetMatch n vars bindExp inExp) =
--       showParen (d>2)
--     $ showString ("let ("++show n++" "++intercalate " " (map showVar vars) ++ ") = ")
--     . shows bindExp . showString " in " . showsPrec 0 inExp
--   showsPrec d (ExpLet i bindExp inExp) =
--       showParen (d>2)
--     $ showString ("let " ++ showVar i ++ " = ")
--     . showsPrec 3 bindExp
--     . showString " in "
--     . showsPrec 0 inExp
--   showsPrec d (ExpCaseMatch bindExp alts) =
--       showParen (d>2)
--     $ showString ("case ")
--     . showsPrec 3 bindExp
--     . showString " of { "
--     . ( \s -> intercalate "; "
--            (map (\(cons, vars, expr) ->
--               show cons++" "++intercalate " " (map showVar vars)++" -> "
--               ++showsPrec 3 expr "")
--             alts)
--          ++ s
--       )
--     . showString " }"

refreshVarTypeBinding :: forall m
                       . MonadMultiState (Map TVarId HsType) m
                      => TVarId
                      -> HsType
                      -> m ()
refreshVarTypeBinding i ty = do
  m <- mGet
  case M.lookup i m of
    Nothing             -> mSet $ M.insert i ty m
    Just TypeVar{}      -> mSet $ M.insert i ty m
    Just TypeConstant{} -> mSet $ M.insert i ty m
    _                   -> return ()

collectVarTypes :: forall m
                 . MonadMultiState (Map TVarId HsType) m
                => Expression
                -> m ()
collectVarTypes (ExpVar i ty)       = refreshVarTypeBinding i ty
collectVarTypes ExpName{}           = return ()
collectVarTypes (ExpLambda i ty se) = do
  refreshVarTypeBinding i ty
  collectVarTypes se
collectVarTypes (ExpApply e1 e2)    = do
  collectVarTypes e1
  collectVarTypes e2
collectVarTypes ExpHole{}           = return ()
collectVarTypes (ExpLetMatch _ vars e1 e2) = do
  vars `forM_` uncurry refreshVarTypeBinding
  collectVarTypes e1
  collectVarTypes e2
collectVarTypes (ExpLet i ty e1 e2) = do
  refreshVarTypeBinding i ty
  collectVarTypes e1
  collectVarTypes e2
collectVarTypes (ExpCaseMatch se matches) = do
  collectVarTypes se
  matches `forM_` \(_, vars, me) -> do
    vars `forM_` uncurry refreshVarTypeBinding
    collectVarTypes me

showExpression :: forall m r w s m2
                . ( m ~ MultiRWST r w s m2
                  , ContainsType QNameIndex s
                  , Monad m2
                  , Functor m2
                  )
               => Expression -> m String
showExpression e = withMultiStateA (M.empty :: Map TVarId HsType)
                 $ [ shs ""
                   | _ <- collectVarTypes e
                   , shs <- h 0 e
                   ]
 where
  h :: Int -> Expression -> MultiRWST r w (Map TVarId HsType ': s) m2 ShowS
  h _ (ExpVar i _) = showString <$> showTypedVar i
  h _ (ExpName s) =
    [ showString
      $ fromMaybe "badNameInternalError"
      $ show <$> maybeQName
    | maybeQName <- lookupQNameId s
    ] 
  h d (ExpLambda i _ e1) =
    [ showParen (d>0) $ showString ("\\" ++ vname ++ " -> ") . eShows
    | eShows <- h 1 e1
    , vname <- showTypedVar i
    ]
  h d (ExpApply e1 e2) =
    [ showParen (d>1) $ e1Shows . showString " " . e2Shows
    | e1Shows <- h 2 e1
    , e2Shows <- h 3 e2
    ]    
  h _ (ExpHole i) = return $ showString $ "_" ++ showVar i
  h d (ExpLetMatch n vars bindExp inExp) =
    [ showParen (d>2)
    $ showString ("let ("
                  ++nStr
                  ++" "
                  ++intercalate " " varNames
                  ++ ") = ")
    . bindShows . showString " in " . inShows
    | bindShows <- h 0 bindExp
    , inShows   <- h 0 inExp
    , nMaybe    <- lookupQNameId n
    , let nStr = fromMaybe "badNameInternalError" $ show <$> nMaybe
    , varNames  <- mapM (showTypedVar . fst) vars
    ]
      
  h d (ExpLet i _ bindExp inExp) =
    [ showParen (d>2)
    $ showString ("let " ++ varName ++ " = ")
    . bindShows
    . showString " in "
    . inShows
    | bindShows <- h 3 bindExp
    , inShows <- h 0 inExp
    , varName <- showTypedVar i
    ]
  h d (ExpCaseMatch bindExp alts) =
    [ showParen (d>2)
    $ showString "case "
    . bindShows
    . showString " of { "
    . ( \s -> intercalate "; "
           (map (\(cons, varNames, expr) ->
              cons ""++" "++intercalate " " varNames++" -> "
              ++expr "")
            altsShows)
         ++ s
      )
    . showString " }"
    | bindShows <- h 3 bindExp
    , altsShows <- alts `forM` \(cons, vars, expr) ->
        [ (showString
          $ fromMaybe "badNameInternalError"
          $ show <$> maybeQName
          , varNames
          , exprShows)
        | maybeQName <- lookupQNameId cons
        , exprShows <- h 3 expr
        , varNames <- mapM (showTypedVar . fst) vars
        ]
    ]

showExpressionPure :: QNameIndex -> Expression -> String
showExpressionPure qNameIndex e = runIdentity
                                $ runMultiRWSTNil
                                $ withMultiStateA qNameIndex
                                $ showExpression e

-- instance Observable Expression where
--   observer x = observeOpaque (show x) x

fillExprHole :: TVarId -> Expression -> Expression -> Expression
fillExprHole vid t orig@(ExpHole j) | vid==j = t
                                    | otherwise = orig
fillExprHole vid t (ExpLambda i ty e) = ExpLambda i ty $ fillExprHole vid t e
fillExprHole vid t (ExpApply e1 e2) = ExpApply (fillExprHole vid t e1)
                                               (fillExprHole vid t e2)
fillExprHole vid t (ExpLetMatch n vars bindExp inExp) =
  ExpLetMatch n vars (fillExprHole vid t bindExp) (fillExprHole vid t inExp)
fillExprHole vid t (ExpLet i ty bindExp inExp) =
  ExpLet i ty (fillExprHole vid t bindExp) (fillExprHole vid t inExp)
fillExprHole vid t (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (fillExprHole vid t bindExp) [ (c, vars, fillExprHole vid t expr)
                                            | (c, vars, expr) <- alts
                                            ]
fillExprHole _ _ t@ExpName{} = t
fillExprHole _ _ t@ExpVar{}  = t


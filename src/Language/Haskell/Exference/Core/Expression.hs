{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonadComprehensions #-}

module Language.Haskell.Exference.Core.Expression
  ( Expression (..)
  , showExpression
  , showExpressionPure
  , fillExprHole
  , simplifyLets
  , simplifyEta
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Data.List ( intercalate )
import Data.Function ( on )
import Data.Maybe ( fromMaybe )
import Control.Monad ( forM )
import Data.Functor.Identity ( runIdentity )

import Control.DeepSeq.Generics
import GHC.Generics

import Control.Monad.Trans.MultiState

import Debug.Hood.Observe



data Expression = ExpVar TVarId -- a
                | ExpName QNameId -- Prelude.zip
                | ExpLambda TVarId Expression -- \x -> exp
                | ExpApply Expression Expression -- f x
                | ExpHole TVarId                 -- h
                | ExpLetMatch QNameId [TVarId] Expression Expression
                            -- let (Foo a b c) = bExp in inExp
                | ExpLet TVarId Expression Expression
                            -- let x = bExp in inExp
                | ExpCaseMatch
                    Expression
                    [(QNameId, [TVarId], Expression)]
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

showExpression :: forall m
                . MonadMultiState QNameIndex m
               => Expression -> m String
showExpression e = ($ "") <$> h 0 e
 where
  h :: Int -> Expression -> m ShowS
  h _ (ExpVar i) = return $ showString $ showVar i
  h _ (ExpName s) =
    [ showString
      $ fromMaybe "badNameInternalError"
      $ show <$> maybeQName
    | maybeQName <- lookupQNameId s
    ] 
  h d (ExpLambda i e1) =
    [ showParen (d>0) $ showString ("\\" ++ showVar i ++ " -> ") . eShows
    | eShows <- h 1 e1
    ]
  h d (ExpApply e1 e2) =
    [ showParen (d>1) $ e1Shows . showString " " . e2Shows
    | e1Shows <- h 2 e1
    , e2Shows <- h 3 e2
    ]    
  h _ (ExpHole i) = return $ showString $ "_" ++ showVar i
  h d (ExpLetMatch n vars bindExp inExp) =
    [ showParen (d>2)
    $ showString ("let ("++nStr++" "++intercalate " " (map showVar vars) ++ ") = ")
    . bindShows . showString " in " . inShows
    | bindShows <- h 0 bindExp
    , inShows   <- h 0 inExp
    , nMaybe    <- lookupQNameId n
    , let nStr = fromMaybe "badNameInternalError" $ show <$> nMaybe
    ]
      
  h d (ExpLet i bindExp inExp) =
    [ showParen (d>2)
    $ showString ("let " ++ showVar i ++ " = ")
    . bindShows
    . showString " in "
    . inShows
    | bindShows <- h 3 bindExp
    , inShows <- h 0 inExp
    ]
  h d (ExpCaseMatch bindExp alts) =
    [ showParen (d>2)
    $ showString ("case ")
    . bindShows
    . showString " of { "
    . ( \s -> intercalate "; "
           (map (\(cons, vars, expr) ->
              cons ""++" "++intercalate " " (map showVar vars)++" -> "
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
          , vars
          , exprShows)
        | maybeQName <- lookupQNameId cons
        , exprShows <- h 3 expr
        ]
    ]

showExpressionPure :: QNameIndex -> Expression -> String
showExpressionPure qNameIndex e = runIdentity
                                $ runMultiStateTNil
                                $ withMultiStateA qNameIndex
                                $ showExpression e

-- instance Observable Expression where
--   observer x = observeOpaque (show x) x

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
fillExprHole vid t (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (fillExprHole vid t bindExp) [ (c, vars, fillExprHole vid t expr)
                                            | (c, vars, expr) <- alts
                                            ]
fillExprHole _ _ t@(ExpName _) = t
fillExprHole _ _ t@(ExpVar _)  = t

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
replaceVar vid t (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (replaceVar vid t bindExp) [ (c, vars, replaceVar vid t expr)
                                          | (c, vars, expr) <- alts
                                          ]
replaceVar _ _ t@(ExpName _) = t
replaceVar _ _ t@(ExpHole _) = t

simplifyLets :: Expression -> Expression
simplifyLets e@(ExpVar _)     = e
simplifyLets e@(ExpName _)    = e
simplifyLets (ExpLambda i e)  = ExpLambda i $ simplifyLets e
simplifyLets (ExpApply e1 e2) = ExpApply (simplifyLets e1) (simplifyLets e2)
simplifyLets e@(ExpHole _)    = e
simplifyLets (ExpLetMatch name vids bindExp inExp) =
  ExpLetMatch name vids (simplifyLets bindExp) (simplifyLets inExp)
simplifyLets (ExpLet i bindExp inExp) = case countUses i inExp of
  0 -> simplifyLets inExp
  1 -> simplifyLets $ replaceVar i bindExp inExp
  _ -> ExpLet i (simplifyLets bindExp) (simplifyLets inExp)
simplifyLets (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (simplifyLets bindExp) [ (c, vars, simplifyLets expr)
                                      | (c, vars, expr) <- alts
                                      ]

countUses :: TVarId -> Expression -> Int
countUses i (ExpVar j) | i==j = 1
                       | otherwise = 0
countUses _ (ExpName _) = 0
countUses i (ExpLambda j e) | i==j = 0
                            | otherwise = countUses i e
countUses i (ExpApply e1 e2) = ((+) `on` countUses i) e1 e2
countUses _ (ExpHole _) = 0
countUses i (ExpLetMatch _ _ bindExp inExp) = ((+) `on` countUses i) bindExp inExp
countUses i (ExpLet _ bindExp inExp) = ((+) `on` countUses i) bindExp inExp
countUses i (ExpCaseMatch bindExp alts) = sum $ countUses i bindExp
                                              : [ countUses i expr
                                                | (_, _, expr) <- alts
                                                ]

simplifyEta :: Expression -> Expression
simplifyEta e@(ExpVar _) = e
simplifyEta e@(ExpName _) = e
simplifyEta (ExpLambda i e) = simplifyEta' $ ExpLambda i $ simplifyEta e
simplifyEta (ExpApply e1 e2) = ExpApply (simplifyEta e1) (simplifyEta e2)
simplifyEta e@(ExpHole _) = e
simplifyEta (ExpLetMatch name vids bindExp inExp) =
  ExpLetMatch name vids (simplifyEta bindExp) (simplifyEta inExp)
simplifyEta (ExpLet i bindExp inExp) =
  ExpLet i (simplifyEta bindExp) (simplifyEta inExp)
simplifyEta (ExpCaseMatch bindExp alts) =
  ExpCaseMatch (simplifyEta bindExp) [ (c, vars, simplifyEta expr)
                                     | (c, vars, expr) <- alts
                                     ]

simplifyEta' :: Expression -> Expression
simplifyEta' (ExpLambda i (ExpApply e1 (ExpVar j)))
  | i==j && 0==countUses i e1 = e1
simplifyEta' e = e

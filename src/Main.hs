{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Main
  ( main
  )
where



import Language.Haskell.Exference
import Language.Haskell.Exference.ExpressionToHaskellSrc
import Language.Haskell.Exference.BindingsFromHaskellSrc
import Language.Haskell.Exference.ContextFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.FunctionBinding
import Language.Haskell.Exference.ContextParser

import Language.Haskell.Exference.ConstrainedType
import Language.Haskell.Exference.SimpleDict
import Language.Haskell.Exference.TypeClasses
import Language.Haskell.Exference.Expression
import Language.Haskell.Exference.ExferenceStats

import Control.DeepSeq

import System.Process

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( second, (***) )
import Control.Monad ( when, forM_, guard, forM, mplus, mzero )
import Data.List ( sortBy, find )
import Data.Ord ( comparing )
import Text.Printf
import Data.Maybe ( listToMaybe, fromMaybe, maybeToList )
import Data.Either ( lefts, rights )
import Control.Monad.Writer.Strict

import Language.Haskell.Exts.Syntax ( Module(..), Decl(..), ModuleName(..) )
import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , parseModule
                                    , ParseResult (..)
                                    , ParseMode (..)
                                    , defaultParseMode )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )

import Data.PPrint

import Control.Arrow ( first )

import MainConfig
import MainTest

import Debug.Hood.Observe



main = runO $ do
  --((eSignatures, StaticContext clss insts), messages) <- runWriter <$> parseExternal testBaseInput'
  (context@(eSignatures, StaticContext clss insts), messages)
    <- runWriter
    <$> contextFromModuleAndRatings "ExferenceDict.hs" "ExferenceRatings.txt"
  -- mapM_ putStrLn $ messages
  -- putStrLn $ replicate 30 '='
  -- printAndStuff testHeuristicsConfig context
  -- printChecks     testHeuristicsConfig context
  -- printStatistics testHeuristicsConfig context
  printCheckedStatistics testHeuristicsConfig context
  -- printMaxUsage testHeuristicsConfig context
  -- printSearchTree testHeuristicsConfig context
  -- mapM_ print $ clss
  -- mapM_ print $ insts
  -- mapM_ print $ eSignatures

  -- print $ compileDict testDictRatings $ eSignatures
  -- print $ parseConstrainedType defaultContext $ "(Show a) => [a] -> String"
  -- print $ inflateConstraints a b
  {-
  print $ constraintMatches testDynContext (badReadVar "y") (read "x")
  print $ constraintMatches testDynContext (badReadVar "x") (read "y")
  print $ constraintMatches testDynContext (badReadVar "w") (read "MyFoo")
  print $ constraintMatches testDynContext (badReadVar "w") (read "MyBar")
  print $ constraintMatches testDynContext (badReadVar "y") (read "MyFoo")
  print $ isProvable testDynContext [Constraint c_applicative [read "y"]]
  -}
  -- putStrLn $ show n ++ " total steps"
  {-
  let t :: HsType
      t = read "m a->( ( a->m b)->( m b))"
  print $ t
  -}

pointfree :: String -> IO String
pointfree s = (!!1) <$> lines <$> readProcess "pointfree" ["--verbose", s] ""

pointful :: String -> IO String
pointful s = (!!0) <$> lines <$> readProcess "pointful" [s] ""

tryParse :: Bool -> String -> IO ()
tryParse shouldBangPattern s = do
  content <- readFile $ "/home/lsp/asd/prog/haskell/exference/BaseContext/preprocessed/"++s++".hs"
  let exts1 = (if shouldBangPattern then (BangPatterns:) else id)
              [ UnboxedTuples
              , TypeOperators
              , MagicHash
              , NPlusKPatterns
              , ExplicitForAll
              , ExistentialQuantification
              , TypeFamilies
              , PolyKinds
              , DataKinds ]
      exts2 = map EnableExtension exts1
  case parseModuleWithMode (ParseMode (s++".hs")
                                      Haskell2010
                                      exts2
                                      False
                                      False
                                      Nothing
                           )
                           content of
    f@(ParseFailed _ _) -> do
      print f
    ParseOk mod -> do
      putStrLn s
      --mapM_ putStrLn $ map (either id show)
      --               $ getBindings defaultContext mod
      --mapM_ putStrLn $ map (either id show)
      --               $ getDataConss mod
      --mapM_ putStrLn $ map (either id show)
      --               $ getClassMethods defaultContext mod

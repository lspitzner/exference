{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Main
  ( main
  )
where



import Language.Haskell.ExferenceCore ( ExferenceHeuristicsConfig(..)
                                      , findExpressionsWithStats )
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
import Language.Haskell.Exference.SearchTree

import Control.DeepSeq

import System.Process

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( first, second, (***) )
import Control.Monad ( when, forM_, guard, forM, mplus, mzero )
import Data.List ( sortBy, find, intersect )
import Data.Ord ( comparing )
import Text.Printf
import Data.Maybe ( listToMaybe, fromMaybe, maybeToList )
import Data.Either ( lefts, rights )
import Control.Monad.Writer.Strict
import qualified Data.Map as M

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
import Data.Tree ( drawTree )


import MainConfig
import MainTest

import Paths_exference
import qualified Paths_exference_core

import System.Environment ( getArgs )
import System.Console.GetOpt
import Data.Version ( showVersion )

import Debug.Hood.Observe



data Flag = Verbose
          | Version
          | Help
          | Tests
          | Examples
          | Env
          | Input String
          | PrintAll
          | PrintTree -- TODO: more options to control core
          | EnvUsage -- TODO: option to specify dictionary to use
          | Serial
          | Unused
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option []    ["version"]     (NoArg Version)              ""
  , Option ['h'] ["help"]        (NoArg Help)                 "prints basic program info"
  , Option ['t'] ["tests"]       (NoArg Tests)                "run the standard validity/performance tests"
  , Option ['x'] ["examples"]    (NoArg Examples)             "prints the first few results for the examples; useful for debugging"
  , Option ['e'] ["environment"] (NoArg Env)                  "print the environment to be used for queries"
  , Option ['v'] ["verbose"]     (NoArg Verbose)              ""
  , Option ['i'] ["input"]       (ReqArg Input "type")        "the type for which to generate an expression"
  , Option ['a'] ["all"]         (NoArg PrintAll)             "print all solutions (up to search step limit)"
  , Option ['u'] ["envUsage"]    (NoArg EnvUsage)             "print a list of functions that got inserted at some point (regardless if successful or not), and how often"
  , Option []    ["tree"]        (NoArg PrintTree)            "print tree of search space"
  , Option ['s'] ["serial"]      (NoArg Serial)               "use the non-parallelized version of the algorithm"
  , Option ['u'] ["allowUnused"] (NoArg Unused)               "allow unused input variables"
  ]

mainOpts :: [String] -> IO ([Flag], [String])
mainOpts argv =
  case getOpt Permute options argv of
    (o, n, []  ) | not $ null $ intersect o [Version, Help, Tests, Examples, Env]
                                ++ [x|x@(Input _) <- o]
                 -> return (o, n)
    ([], _, [])   -> return ([Tests],[])
    (_,  _, errs) -> ioError (userError (concat errs ++ fullUsageInfo))

fullUsageInfo = usageInfo header options
  where
    header = "Usage: exference [OPTION...]"

main = runO $ do
  argv <- getArgs
  (flags, params) <- mainOpts argv
  let verbose = Verbose `elem` flags
  let
    printVersion = do
      putStrLn $ "exference version " ++ showVersion version
      putStrLn $ "exference-core version " ++ showVersion Paths_exference_core.version
  if | [Version] == flags   -> printVersion
     | Help    `elem` flags -> putStrLn $ "TODO"
     | otherwise -> do
        when (Version `elem` flags || verbose) printVersion
        --((eSignatures, StaticContext clss insts), messages) <- runWriter <$> parseExternal testBaseInput'
        when verbose $ do
          putStrLn "[Environment]"
          putStrLn "reading context from ExferenceDict.hs and ExferenceRatings.txt"
        (context@(eSignatures, scontext@(StaticContext clss insts)), messages)
          <- runWriter
          <$> contextFromModuleAndRatings "ExferenceDict.hs" "ExferenceRatings.txt"
        when (verbose && not (null messages)) $ do
          forM_ messages $ \m -> putStrLn $ "environment warning: " ++ m
        when (Env `elem` flags) $ do
          when verbose $ putStrLn "[Environment]"
          mapM_ print $ clss
          mapM_ print $ [(i,x)| (i,xs) <- M.toList insts, x <- xs]
          mapM_ print $ eSignatures
        when (Examples `elem` flags) $ do
          when verbose $ putStrLn "[Examples]"
          printAndStuff testHeuristicsConfig context
        when (Tests `elem` flags) $ do
          when verbose $ putStrLn "[Tests]"
          printCheckExpectedResults (not $ Serial `elem` flags)
                                    testHeuristicsConfig
                                    context
        case [x|(Input x)<-flags] of
          []    -> return ()
          (x:_) -> do
            when verbose $ putStrLn "[Custom Input]"
            let input = ExferenceInput
                  (readConstrainedType scontext x)
                  (filter (\(x,_,_) -> x/="join" && x/="liftA2") eSignatures)
                  scontext
                  (Unused `elem` flags)
                  131072
                  (Just 131072)
                  testHeuristicsConfig
            if
              | PrintAll `elem` flags -> do
                  when verbose $ putStrLn "[running findExpressions ..]"
                  let rs = findExpressions input
                  if null rs
                    then putStrLn "[no results]"
                    else forM_ rs
                      $ \(e, ExferenceStats n d) ->
                        putStrLn $ show e
                                    ++ " (depth " ++ show d
                                    ++ ", " ++ show n ++ " steps)"
              | PrintTree `elem` flags -> do
                  when verbose $ putStrLn "[running findExpressionsWithStats ..]"
                  let (_, tree, _) = last $ findExpressionsWithStats input
                  let showf (total,processed,expression,_)
                        = printf "%d (+%d): %s" processed
                                                (total-processed)
                                                (show expression)
                  putStrLn $ drawTree
                           $ fmap showf
                           -- $ filterSearchTreeProcessedN 2
                           $ tree
              | EnvUsage `elem` flags -> do
                  when verbose $ putStrLn "[running findExpressionsWithStats ..]"
                  let (stats, _, _) = last $ findExpressionsWithStats input
                      highest = take 8 $ sortBy (flip $ comparing snd) $ M.toList stats
                  putStrLn $ show highest
              | otherwise -> do
                  r <- if Serial `elem` flags
                    then do
                      when verbose $ putStrLn "[running findOneExpression ..]"
                      return $ findOneExpression input
                    else do
                      when verbose $ putStrLn "[running findOneExpressionPar ..]"
                      findOneExpressionPar input
                  case r of
                    Nothing -> putStrLn "[no result]"
                    Just (e, ExferenceStats n d) ->
                        putStrLn $ show e
                                    ++ " (depth " ++ show d
                                    ++ ", " ++ show n ++ " steps)"

        -- printChecks     testHeuristicsConfig context
        -- printStatistics testHeuristicsConfig context
        
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

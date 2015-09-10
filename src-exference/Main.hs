{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}

module Main
  ( main
  )
where



import Language.Haskell.Exference.Core ( ExferenceHeuristicsConfig(..)
                                      , findExpressionsWithStats )
import Language.Haskell.Exference
import Language.Haskell.Exference.ExpressionToHaskellSrc
import Language.Haskell.Exference.BindingsFromHaskellSrc
import Language.Haskell.Exference.ClassEnvFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.EnvironmentParser

import Language.Haskell.Exference.SimpleDict
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.ExpressionSimplify
import Language.Haskell.Exference.Core.ExferenceStats
import Language.Haskell.Exference.Core.SearchTree

import Control.DeepSeq

import System.Process hiding ( env )

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( first, second, (***) )
import Control.Monad ( when, forM_, guard, forM, mplus, mzero )
import Data.List ( sortBy, find, intersect, intersperse, intercalate, nub )
import Data.Ord ( comparing )
import Text.Printf
import Data.Maybe ( listToMaybe, fromMaybe, maybeToList )
import Data.Either ( lefts, rights )
import Data.Functor.Identity ( runIdentity )
import Control.Monad.Writer.Strict
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IntMap

import Language.Haskell.Exts.Syntax ( Module(..), Decl(..), ModuleName(..) )
import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , parseModule
                                    , ParseResult (..)
                                    , ParseMode (..)
                                    , defaultParseMode )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )
import Language.Haskell.Exts.Pretty

import Control.Monad.Trans.MultiRWS
import Control.Monad.Trans.Either

import Data.PPrint
import Data.Tree ( Tree(..) )


import MainConfig
import MainTest

import Paths_exference
import qualified Flags_exference

import System.Environment ( getArgs )
import System.Console.GetOpt
import Data.Version ( showVersion )

import Debug.Hood.Observe



data Flag = Verbose Int
          | Version
          | Help
          | Tests
          | Examples
          | PrintEnv
          | EnvDir String
          | Input String
          | PrintAll
          | PrintTree -- TODO: more options to control core
          | EnvUsage -- TODO: option to specify dictionary to use
          | Serial
          | Parallel
          | Shortest
          | FirstSol
          | Best
          | Unused
          | PatternMatchMC
          | Qualification Int
          | Constraints
          | AllowFix
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option []    ["version"]     (NoArg Version)       ""
  , Option []    ["help"]        (NoArg Help)          "prints basic program info"
  , Option ['t'] ["tests"]       (NoArg Tests)         "run the standard validity/performance tests"
  , Option ['x'] ["examples"]    (NoArg Examples)      "prints the first few results for the examples; useful for debugging"
  , Option ['p'] ["printenv"]    (NoArg PrintEnv)      "print the environment to be used for queries"
  , Option ['e'] ["envdir"]      (ReqArg EnvDir "PATH") "path to environment directory"
  , Option ['v'] ["verbose"]     (OptArg (Verbose . maybe 1 read) "INT") "verbosity"
  , Option ['i'] ["input"]       (ReqArg Input "HSTYPE") "the type for which to generate an expression"
  , Option ['a'] ["all"]         (NoArg PrintAll)      "print all solutions (up to search step limit)"
  , Option []    ["envUsage"]    (NoArg EnvUsage)      "print a list of functions that got inserted at some point (regardless if successful or not), and how often"
  , Option []    ["tree"]        (NoArg PrintTree)     "print tree of search space"
  , Option []    ["serial"]      (NoArg Serial)        "use the non-parallelized version of the algorithm (default)"
  , Option ['j'] ["parallel"]    (NoArg Parallel)      "use the parallelized version of the algorithm"
  , Option ['o'] ["short"]       (NoArg Shortest)      "prefer shorter solutions"
  , Option ['f'] ["first"]       (NoArg FirstSol)      "stop after finding the first solution"
  , Option []    ["fix"]         (NoArg AllowFix)      "allow the `fix` function in the environment"
  , Option ['b'] ["best"]        (NoArg Best)          "calculate all solutions, and print the best one"
  , Option ['u'] ["allowUnused"] (NoArg Unused)        "allow unused input variables"
  , Option ['c'] ["patternMatchMC"] (NoArg PatternMatchMC) "pattern match on multi-constructor data types (might lead to hang-ups at the moment)"
  , Option ['q'] ["fullqualification"] (NoArg $ Qualification 2) "fully qualify the identifiers in the output"
  , Option []    ["somequalification"] (NoArg $ Qualification 1) "fully qualify non-operator-identifiers in the output"
  , Option ['w'] ["allowConstraints"] (NoArg Constraints) "allow additional (unproven) constraints in solutions"
  ]

mainOpts :: [String] -> IO ([Flag], [String])
mainOpts argv =
  case getOpt Permute options argv of
    (o, n, []  )  | inputs <- [x | (Input x) <- o] ++ n
                  -> if null (intersect o [Version, Help, Tests, Examples, PrintEnv])
                     && null inputs
                    then return (Tests:o, inputs)
                    else return (o      , inputs)
    (_,  _, errs) -> ioError (userError (concat errs ++ fullUsageInfo))

fullUsageInfo :: String
fullUsageInfo = usageInfo header options
  where
    header = "Usage: exference [OPTION...]"

main :: IO ()
main = runO $ do
  argv <- getArgs
  defaultEnvPath <- getDataFileName "environment"
  (flags, inputs) <- mainOpts argv
  let verbosity = sum $ [x | Verbose x <- flags ]
  let qualification = head $ [x | Qualification x <- flags] ++ [0]
  let
    printVersion = do
      putStrLn $ "exference version " ++ showVersion version
  if | [Version] == flags   -> printVersion
     | Help    `elem` flags -> putStrLn fullUsageInfo >> putStrLn "TODO"
     | otherwise -> runMultiRWSTNil_ $ withQNameIndex $ do
        par <- case (Parallel `elem` flags, Serial `elem` flags) of
          (False,False) -> return False
          (True, False) -> return True
          (False,True ) -> return False
          (True, True ) -> do
            error "--serial and --parallel are in conflict! aborting" 
        when (Version `elem` flags || verbosity>0) $ lift printVersion
        -- ((eSignatures, StaticClassEnv clss insts), messages) <- runWriter <$> parseExternal testBaseInput'
        let envDir = case [d | EnvDir d <- flags] of
                       []    -> defaultEnvPath
                       (d:_) -> d
        when (verbosity>0) $ lift $ do
          putStrLn $ "[Environment]"
          putStrLn $ "reading environment from " ++ envDir
        ( (eSignatures
          , eDeconss
          , sEnv@(StaticClassEnv clss insts)
          , validNames
          , tdeclMap )
         ,messages :: [String] ) <- withMultiWriterAW $ environmentFromPath envDir
        let
          env = (eSignatures, eDeconss, sEnv)
        when (verbosity>0 && not (null messages)) $ lift $ do
          forM_ messages $ \m -> putStrLn $ "environment warning: " ++ m
        when (PrintEnv `elem` flags) $ lift $ do
          when (verbosity>0) $ putStrLn "[Environment]"
          mapM_ print $ IntMap.elems tdeclMap
          mapM_ print $ clss
          mapM_ print $ [(i,x)| (i,xs) <- IntMap.toList insts, x <- xs]
          mapM_ print $ eSignatures
          mapM_ print $ eDeconss
        when (verbosity>0) $ showQNameIndex >>= mapM_ (lift . putStrLn)
        when (Examples `elem` flags) $ do
          when (verbosity>0) $ lift $ putStrLn "[Examples]"
          printAndStuff testHeuristicsConfig env
        when (Tests `elem` flags) $ do
          when (verbosity>0) $ lift $ putStrLn "[Tests]"
          withMultiReader tdeclMap $ printCheckExpectedResults
                                       testHeuristicsConfig { heuristics_solutionLength = 0.0 }
                                       env
        case inputs of
          []    -> return () -- probably impossible..
          (x:_) -> do
            when (verbosity>0) $ lift $ putStrLn "[Custom Input]"
            eParsedType <- runEitherT $ parseType (sClassEnv_tclasses sEnv)
                                                  Nothing
                                                  validNames
                                                  tdeclMap
                                                  (haskellSrcExtsParseMode "inputtype")
                                                  x
            case eParsedType of
              Left err -> lift $ do
                putStrLn $ "could not parse input type: " ++ err
              Right (parsedType, tVarIndex) -> do
                typeStr <- showHsType tVarIndex parsedType
                when (verbosity>0) $ lift $ putStrLn $ "input type parsed as: " ++ typeStr
                unresolvedIdents <- findInvalidNames validNames parsedType
                when (not $ null unresolvedIdents) $ lift $ do
                  putStrLn $ "warning: unresolved idents in input: "
                           ++ intercalate ", " (nub $ show <$> unresolvedIdents)
                  putStrLn $ "(this may be harmless, but no instances will be connected to these.)"
                qNameIndex <- mGet
                let hidden = if AllowFix `elem` flags then [] else ["fix", "forever", "iterateM_"]
                let filteredBindings = runIdentity
                                     $ runMultiRWSTNil
                                     $ withMultiStateA qNameIndex
                                     $ filterBindingsSimple hidden eSignatures
                let input = ExferenceInput
                      parsedType
                      filteredBindings
                      eDeconss
                      sEnv
                      (Unused `elem` flags)
                      (Constraints `elem` flags)
                      8192
                      (PatternMatchMC `elem` flags)
                      qNameIndex
                      65536
                      (Just 8192)
                      (if Shortest `elem` flags then
                         testHeuristicsConfig
                       else
                         testHeuristicsConfig { heuristics_solutionLength = 0.0 })
                when (verbosity>0) $ lift $ do
                  putStrLn $ "full input:"
                  doc <- pprint input
                  print doc
                if
                  | PrintAll `elem` flags -> do
                      when (verbosity>0) $ lift $ putStrLn "[running findExpressions ..]"
                      let rs = findExpressions input
                      if null rs
                        then lift $ putStrLn "[no results]"
                        else forM_ rs
                          $ \(e, constrs, ExferenceStats n d m) -> do
                            hsE <- convert qualification $ simplifyExpression e
                            lift $ putStrLn $ prettyPrint hsE
                            when (not $ null constrs) $ do
                              constrStrs <- mapM (showHsConstraint tVarIndex)
                                          $ S.toList
                                          $ S.fromList
                                          $ constrs
                              lift $ putStrLn $ "but only with additional contraints: " ++ intercalate ", " constrStrs
                            lift $ putStrLn $ replicate 40 ' ' ++ "(depth " ++ show d
                                        ++ ", " ++ show n ++ " steps, " ++ show m ++ " max pqueue size)"
                  | PrintTree `elem` flags ->
                      if not Flags_exference.buildSearchTree
                        then lift $ putStrLn "exference-core was not compiled with flag \"buildSearchTree\""
                        else do
                          when (verbosity>0) $ lift $ putStrLn "[running findExpressionsWithStats ..]"
                          let (_, tree, _) = last $ findExpressionsWithStats
                                                  $ input {input_maxSteps = 8192}
                          let showf (total,processed,expression,_)
                                = ( printf "%d (+%d):" processed (total-processed)
                                  , showExpressionPure qNameIndex $ simplifyExpression expression
                                  )
                          let
                            helper :: String -> Tree (String, String) -> [String]
                            helper indent (Node (n,m) ts) =
                              (printf "%-50s %s" (indent ++ n) m)
                              : concatMap (helper ("  "++indent)) ts
                          (lift . putStrLn) `mapM_` helper "" (showf <$> filterSearchTreeProcessedN 64 tree)
                          -- putStrLn . showf `mapM_` draw
                          --   -- $ filterSearchTreeProcessedN 2
                          --   tree
                  | EnvUsage `elem` flags -> lift $ do
                      when (verbosity>0) $ putStrLn "[running findExpressionsWithStats ..]"
                      let (stats, _, _) = last $ findExpressionsWithStats input
                          highest = take 8 $ sortBy (flip $ comparing snd) $ M.toList stats
                      putStrLn $ show $ highest
                  | otherwise -> do
                      r <- if
                        | FirstSol `elem` flags -> if par
                          then lift $ do
                            putStrLn $ "WARNING: parallel version not implemented for given flags, falling back to serial!"
                            when (verbosity>0) $ putStrLn "[running findOneExpression ..]"
                            return $ maybeToList $ findOneExpression input
                          else lift $ do 
                            when (verbosity>0) $ putStrLn "[running findOneExpression ..]"
                            return $ maybeToList $ findOneExpression input
                        | Best `elem` flags -> if par
                          then lift $ do
                            putStrLn $ "WARNING: parallel version not implemented for given flags, falling back to serial!"
                            when (verbosity>0) $ putStrLn "[running findBestNExpressions ..]"
                            return $ findBestNExpressions 999 input
                          else lift $ do
                            when (verbosity>0) $ putStrLn "[running findBestNExpressions ..]"
                            return $ findBestNExpressions 999 input
                        | otherwise -> if par
                          then lift $ do
                            putStrLn $ "WARNING: parallel version not implemented for given flags, falling back to serial!"
                            when (verbosity>0) $ putStrLn "[running findFirstBestExpressionsLookaheadPreferNoConstraints ..]"
                            return $ findFirstBestExpressionsLookaheadPreferNoConstraints 256 input
                          else lift $ do
                            if Constraints `elem` flags
                              then do
                                when (verbosity>0) $ putStrLn "[running findFirstBestExpressionsLookahead ..]"
                                return $ findFirstBestExpressionsLookahead 256 input
                              else do
                                when (verbosity>0) $ putStrLn "[running findFirstBestExpressionsLookaheadPreferNoConstraints ..]"
                                return $ findFirstBestExpressionsLookaheadPreferNoConstraints 256 input {input_allowConstraints = True}
                      case r :: [ExferenceOutputElement] of
                        [] -> lift $ putStrLn "[no results]"
                        rs -> rs `forM_` \(e, constrs, ExferenceStats n d m) -> do
                            hsE <- convert qualification $ simplifyExpression e
                            lift $ putStrLn $ prettyPrint hsE
                            when (not $ null constrs) $ do
                              constrStrs <- mapM (showHsConstraint tVarIndex)
                                          $ S.toList
                                          $ S.fromList
                                          $ constrs
                              lift $ putStrLn $ "but only with additional contraints: " ++ intercalate ", " constrStrs
                            lift $ putStrLn $ replicate 40 ' ' ++ "(depth " ++ show d
                                       ++ ", " ++ show n ++ " steps, " ++ show m ++ " max pqueue size)"

        -- printChecks     testHeuristicsConfig env
        -- printStatistics testHeuristicsConfig env
        
        -- print $ compileDict testDictRatings $ eSignatures
        -- print $ parseConstrainedType defaultClassEnv $ "(Show a) => [a] -> String"
        -- print $ inflateHsConstraints a b
        {-
        let t :: HsType
            t = read "m a->( ( a->m b)->( m b))"
        print $ t
        -}

_pointfree :: String -> IO String
_pointfree s = (!!1) <$> lines <$> readProcess "pointfree" ["--verbose", s] ""

_pointful :: String -> IO String
_pointful s = (!!0) <$> lines <$> readProcess "pointful" [s] ""

_tryParse :: Bool -> String -> IO ()
_tryParse shouldBangPattern s = do
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
    ParseOk _modul -> do
      putStrLn s
      --mapM_ putStrLn $ map (either id show)
      --               $ getBindings defaultClassEnv mod
      --mapM_ putStrLn $ map (either id show)
      --               $ getDataConss mod
      --mapM_ putStrLn $ map (either id show)
      --               $ getClassMethods defaultClassEnv mod

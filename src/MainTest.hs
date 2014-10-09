module MainTest
  ( printAndStuff
  , printChecks
  , printStatistics
  , printCheckedStatistics
  , printMaxUsage
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

import Language.Haskell.Exference.ConstrainedType
import Language.Haskell.Exference.TypeClasses
import Language.Haskell.Exference.Expression
import Language.Haskell.Exference.ExferenceStats

import Control.DeepSeq

import System.Process

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( second, (***) )
import Control.Monad ( when, forM_, guard, forM, mplus, mzero )
import Data.List ( sortBy, find, intercalate )
import Data.Ord ( comparing )
import Text.Printf
import Data.Maybe ( listToMaybe, fromMaybe, maybeToList, catMaybes )
import Data.Either ( lefts, rights )
import Control.Monad.Writer.Strict
import qualified Data.Map as M

import Language.Haskell.Exts.Syntax ( Module(..), Decl(..), ModuleName(..) )
import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , parseModule
                                    , ParseResult (..)
                                    , ParseMode (..) )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )

import Data.PPrint

import Debug.Hood.Observe



checkData :: [(String, Bool, String, [String])]
checkData =
  [ (,,,) "showmap"    False "(Show b) => (a -> b) -> List a -> List String"
                             ["\\b -> fmap (\\g -> show (b g))"]
  , (,,,) "ffbind"     False "(a -> t -> b) -> (t -> a) -> (t -> b)"
                             ["\\b -> (\\c -> (\\d -> (b (c d)) d))"]
  , (,,,) "join"       False "(Monad m) => m (m a) -> m a"
                             ["\\b -> ((>>=) b) (\\f -> f)"]
  , (,,,) "fjoin"      False "(t -> (t -> a)) -> t -> a"
                             ["\\b -> (\\c -> (b c) c)"]
  , (,,,) "zipThingy"  False "List a -> b -> List (Tuple2 a b)"
                             ["\\b -> (\\c -> ((fmap (\\g -> ((,) g) c)) b)"
                             ,"\\b -> (\\c -> (zip b) (pure c))"]
  , (,,,) "stateRun"   True  "State a b -> a -> b"
                             ["\\b -> (\\c -> let (State e) = b in let ((,) h i) = e c in h)"]
  , (,,,) "fst"        True  "Tuple2 a b -> a"
                             ["\\b -> let ((,) d e) = b in d"]
  --, (,,,) "ffst"       True  "(a -> Tuple b c) -> a -> b"
  , (,,,) "snd"        True  "Tuple2 a b -> b"
                             ["\\b -> let ((,) d e) = b in e"]
  , (,,,) "quad"       False "a -> Tuple2 (Tuple2 a a) (Tuple2 a a)"
                             ["\\b -> ((,) (((,) b) b)) (((,) b) b)"]
  -- , (,,,) "fswap"      False "(a -> Tuple b c) -> a -> Tuple c b"
  , (,,,) "liftBlub"   False "Monad m => m a -> m b -> (a -> b -> m c) -> m c"
                             ["\\b -> (\\c -> (\\d -> ((>>=) b) (\\h -> ((>>=) c) (d h))))"
                             ,"\\b -> (\\c -> (\\d -> ((>>=) c) (\\h -> ((>>=) b) (\\l -> (d l) h))))"]
  , (,,,) "stateBind"  False "State s a -> (a -> State s b) -> State s b"
                             ["\\b -> (\\c -> let (State e) = b in State (\\g -> let ((,) k l) = e g in let (State o) = c k in o l))"]
  , (,,,) "dbMaybe"    False "Maybe a -> Maybe (Tuple2 a a)"
                             ["fmap (\\f -> ((,) f) f)"
                             ,"\\b -> ((liftM2 (\\g -> (\\h -> ((,) h) g))) b) b"
                             ,"\\b -> ((>>=) b) (\\f -> pure (((,) f) f))"]
  , (,,,) "tupleShow"  False "Show a, Show b => Tuple2 a b -> String"
                             ["show"
                             ,"\\b -> let ((,) d e) = b in show (((,) d) e)"]
  , (,,,) "FloatToInt" False "Float -> Int"
                             ["truncate"]
  , (,,,) "liftSBlub"  False "Monad m, Monad n => (List a -> b -> c) -> m (List (n a)) -> m (n b) -> m (n c)"
                             []
  --, (,,,) "longApp"    False "a -> b -> c -> (a -> b -> d) -> (a -> c -> e) -> (b -> c -> f) -> (d -> e -> f -> g) -> g"
  --, (,,,) "liftSBlub"  False "Monad m, Monad n => (List a -> b -> c) -> m (List (n a)) -> m (n b) -> m (n c)"
  --, (,,,) "liftSBlubS" False "Monad m => (List a -> b -> c) -> m (List (Maybe a)) -> m (Maybe b) -> m (Maybe c)"
  --, (,,,) "joinBlub"   False "Monad m => List Decl -> (Decl -> m (List FunctionBinding)) -> m (List FunctionBinding)"
  ]

{-
  , (,) "liftBlub"
    (ExpLambda 1
      (ExpLambda 2
        (ExpLambda 3
          (ExpApply
            (ExpApply (ExpLit "(>>=)") (ExpVar 1))
            (ExpLambda 7
              (ExpApply
                (ExpApply (ExpLit "(>>=)") (ExpVar 2))
                (ExpLambda 11
                  (ExpApply
                    (ExpApply (ExpVar 3) (ExpVar 7))
                    (ExpVar 11)))))))))
-}

exampleInput :: [(String, Bool, String)]
exampleInput = 
  [ (,,) "State"      False "(s -> Tuple2 a s) -> State s a"
  , (,,) "showmap"    False "(Show b) => (a -> b) -> List a -> List String"
  , (,,) "ffbind"     False "(a -> t -> b) -> (t -> a) -> (t -> b)"
  , (,,) "join"       False "(Monad m) => m (m a) -> m a"
  , (,,) "fjoin"      False "(t -> (t -> a)) -> t -> a"
  , (,,) "zipThingy"  False "List a -> b -> List (Tuple2 a b)"
  , (,,) "stateRun"   True  "State a b -> a -> b"
  , (,,) "fst"        True  "Tuple2 a b -> a"
  , (,,) "ffst"       True  "(a -> Tuple2 b c) -> a -> b"
  , (,,) "snd"        True  "Tuple2 a b -> b"
  , (,,) "quad"       False "a -> Tuple2 (Tuple2 a a) (Tuple2 a a)"
  , (,,) "fswap"      False "(a -> Tuple2 b c) -> a -> Tuple2 c b"
  , (,,) "liftBlub"   False "Monad m => m a -> m b -> (a -> b -> m c) -> m c"
  , (,,) "stateBind"  False "State s a -> (a -> State s b) -> State s b"
  , (,,) "dbMaybe"    False "Maybe a -> Maybe (Tuple2 a a)"
  , (,,) "tupleShow"  False "Show a, Show b => Tuple2 a b -> String"
  , (,,) "FloatToInt" False "Float -> Int"
  , (,,) "longApp"    False "a -> b -> c -> (a -> b -> d) -> (a -> c -> e) -> (b -> c -> f) -> (d -> e -> f -> g) -> g"
  , (,,) "liftSBlub"  False "Monad m, Monad n => (List a -> b -> c) -> m (List (n a)) -> m (n b) -> m (n c)"
  , (,,) "liftSBlubS" False "Monad m => (List a -> b -> c) -> m (List (Maybe a)) -> m (Maybe b) -> m (Maybe c)"
  , (,,) "joinBlub"   False "Monad m => List Decl -> (Decl -> m (List FunctionBinding)) -> m (List FunctionBinding)"
  ]

checkResults :: ExferenceHeuristicsConfig
             -> Context
             -> [IO ( String -- name
                    , [String] -- expected
                    , Maybe Expression -- first
                    , Maybe Expression -- best
                    , Maybe (Int, ExferenceStats)    -- no idea atm
                    )]
checkResults heuristics (bindings, scontext) = do
  (name, allowUnused, typeStr, expected) <- checkData
  let input = ExferenceInput
                (readConstrainedType scontext typeStr)
                (filter (\(x,_,_) -> x/="join") bindings)
                scontext
                allowUnused
                32768
                (Just 32768)
                heuristics
  let r = findExpressions input
  let finder :: Int
             -> [(Expression, ExferenceStats)]
             -> Maybe (Int, ExferenceStats)
      finder n [] = Nothing
      finder n ((e, s):r) | show e `elem` expected = Just (n, s)
                          | otherwise = finder (n+1) r
      bestFound = findSortNExpressions 100 input
  return $ (,,,,)
         <$> return name
         <*> return expected
         <*> fmap fst <$> findOneExpressionPar input
         -- <*> return (fst <$> findOneExpression input)
         <*> return (fst <$> listToMaybe bestFound)
         <*> return (finder 0 r)

exampleOutput :: ExferenceHeuristicsConfig
              -> Context
              -> [[(Expression, ExferenceStats)]]
exampleOutput heuristics (bindings, scontext) = map f exampleInput
  where
    input = ExferenceInput
    f (_, allowUnused, s) = takeFindSortNExpressions 5 10 $ ExferenceInput
                (readConstrainedType scontext s)
                (filter (\(x,_,_) -> x/="join") bindings)
                scontext
                allowUnused
                32768
                (Just 32768)
                heuristics

exampleInOut h context = zip exampleInput (exampleOutput h context)

printAndStuff h context = mapM_ f (exampleInOut h context)
  where
    f ((name, _, _), []) = putStrLn $ "no results for "++name++"!"
    f ((name, _, _), results) = mapM_ g results
      where
        g (expr, ExferenceStats n d) = do
          let str = show expr
              doPf = False
          if doPf then do
            pf <- pointfree $ str
            putStrLn $ name ++ " = " ++ pf
                       ++ "    FROM    " ++ name ++ " = " ++ str
                       ++ " (depth " ++ show d ++ ", " ++ show n ++ " steps)"
           else
            putStrLn $ name ++ " = " ++ str
                       ++ " (depth " ++ show d ++ ", " ++ show n ++ " steps)"

printStatistics h context = mapM_ f (exampleInOut h context)
  where
    f ((name, _, _), [])      = putStrLn $ printf "%10s: ---" name
    f ((name, _, _), results) =
      let (hd, avg, min, max, n) = getStats results
      in putStrLn $ printf "%10s: head=%6d avg=%6d min=%6d max=%6d %s" name hd avg min max
                                     (if n==6 then "" else " n = " ++ show n)
    getStats results =
      let steps = map (exference_steps.snd) results
      in ( head steps
         , sum steps `div` length steps
         , minimum steps
         , maximum steps
         , length steps
         )

printChecks :: ExferenceHeuristicsConfig -> Context -> IO ()
printChecks h context = (>>=helper) `mapM_` checkResults h context
  where
    helper :: ( String
              , [String]
              , Maybe Expression
              , Maybe Expression
              , Maybe (Int, ExferenceStats))
           -> IO ()
    helper (name, _, Just f, Just b, Just (0, ExferenceStats n d))
      | f==b = do putStrLn $ printf "%-10s: fine                                  %5d %8.2f" name n d
      | otherwise = do
      putStrLn $ printf "%-10s: expected solution first, but not best!" name
      putStrLn $ "  expected solution:  " ++ show f
      putStrLn $ "  best solution:      " ++ show b
    helper (name, e, Just f, _, Just (i, _)) = do
      putStrLn $ printf "%-10s: expected solution not first, but %d!" name i
      putStrLn $ "  first solution:     " ++ show f
      putStrLn $ "  expected solutions: " ++ intercalate ", " e
    helper (name, e, Just f, Just b, Nothing) = do
      putStrLn $ printf "%-10s: expected solution not found!" name
      putStrLn $ "  first solution was  " ++ show f
      putStrLn $ "  best solution:      " ++ show b
      putStrLn $ "  expected solutions: " ++ intercalate ", " e
    helper (name, e, Just f, Nothing, Nothing) = do -- this can't really happen..
      putStrLn $ printf "%-10s: expected solution not found!" name
      putStrLn $ "  first solution was  " ++ show f
      putStrLn $ "  expected solutions: " ++ intercalate ", " e
    helper (name, _, Nothing, _, _) = do
      putStrLn $ printf "%-10s: no solutions found at all!" name 

printCheckedStatistics :: ExferenceHeuristicsConfig -> Context -> IO ()
printCheckedStatistics h context = do
  xs <- mapM (>>=f) (checkResults h context)
  print $ foldr g (0, 0, 0.0) $ catMaybes $ xs
  where
    f (name, expected, Just first, _, Just (n, stats)) = case n of
      0 -> do
        putStrLn $ printf "%-10s: %s" name (show stats)
        return (Just stats)
      _ -> do
        putStrLn $ printf "%-10s: bad (not first), first is %s" name (show first)
        return Nothing
    f (name, expected, Just first, _, _) = do
        putStrLn $ printf "%-10s: bad (only different solution found), first is %s" name (show first)
        return Nothing
    f (name, expected, _, _, _) = do
        putStrLn $ printf "%-10s: bad (no solution)" name
        return Nothing
    g :: ExferenceStats -> (Int,Int,Float) -> (Int,Int,Float)
    g (ExferenceStats a b) (c,d,e) = (c+1,a+d,b+e)

printMaxUsage :: ExferenceHeuristicsConfig -> Context -> IO ()
printMaxUsage h (bindings, scontext) = mapM_ f checkData
  where
    f (name, allowUnused, typeStr, _expected) = do
      let input = ExferenceInput
                    (readConstrainedType scontext typeStr)
                    (filter (\(x,_,_) -> x/="join") bindings)
                    scontext
                    allowUnused
                    16384
                    (Just 16384)
                    h
      let (stats, _) = last $ findExpressionsWithStats input
          highest = take 5 $ sortBy (flip $ comparing snd) $ M.toList stats
      putStrLn $ printf "%-10s: %s" name (show highest)

-- TODO: remove duplication
pointfree :: String -> IO String
pointfree s = (!!1) <$> lines <$> readProcess "pointfree" ["--verbose", s] ""

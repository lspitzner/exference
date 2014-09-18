module MainTest
  ( printAndStuff
  , printChecks
  , printStatistics
  )
where



import Language.Haskell.Exference
import Language.Haskell.Exference.ExpressionToHaskellSrc
import Language.Haskell.Exference.BindingsFromHaskellSrc
import Language.Haskell.Exference.ContextFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.FunctionBinding

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
                                    , ParseMode (..) )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )

import Data.PPrint

import Debug.Hood.Observe



testInput :: [(String, Bool, String)]
testInput = 
  [ (,,) "showmap"    False "(Show b) => (a -> b) -> List a -> List String"
  , (,,) "ffbind"     False "(a -> t -> b) -> (t -> a) -> (t -> b)"
  , (,,) "join"       False "(Monad m) => m (m a) -> m a"
  , (,,) "fjoin"      False "(t -> (t -> a)) -> t -> a"
  , (,,) "zipThingy"  False "List a -> b -> List (Tuple a b)"
  , (,,) "stateRun"   True  "State a b -> a -> b"
  , (,,) "fst"        True  "Tuple a b -> a"
  , (,,) "ffst"       True  "(a -> Tuple b c) -> a -> b"
  , (,,) "snd"        True  "Tuple a b -> b"
  , (,,) "quad"       False "a -> Tuple (Tuple a a) (Tuple a a)"
  , (,,) "fswap"      False "(a -> Tuple b c) -> a -> Tuple c b"
  , (,,) "liftBlub"   False "Monad m => m a -> m b -> (a -> b -> m c) -> m c"
  , (,,) "stateBind"  False "State s a -> (a -> State s b) -> State s b"
  , (,,) "dbMaybe"    False "Maybe a -> Maybe (Tuple a a)"
  , (,,) "tupleShow"  False "Show a, Show b => Tuple a b -> String"
  , (,,) "FloatToInt" False "Float -> Int"
  , (,,) "longApp"    False "a -> b -> c -> (a -> b -> d) -> (a -> c -> e) -> (b -> c -> f) -> (d -> e -> f -> g) -> g"
  , (,,) "liftSBlub"  False "Monad m, Monad n => (List a -> b -> c) -> m (List (n a)) -> m (n b) -> m (n c)"
  , (,,) "liftSBlubS" False "Monad m => (List a -> b -> c) -> m (List (Maybe a)) -> m (Maybe b) -> m (Maybe c)"
  , (,,) "joinBlub"   False "Monad m => List Decl -> (Decl -> m (List FunctionBinding)) -> m (List FunctionBinding)"
  ]

expected :: [(String, Expression)]
expected =
  [ (,) "showmap"
    (ExpLambda 1
      (ExpLambda 2
        (ExpApply
          (ExpApply
            (ExpLit "fmap")
            (ExpLambda 6
              (ExpApply
                (ExpLit "show")
                (ExpApply (ExpVar 1) (ExpVar 6)))))
          (ExpVar 2))))
  , (,) "ffbind"
    (ExpLambda 1
      (ExpLambda 2
        (ExpLambda 3
          (ExpApply
            (ExpApply
              (ExpVar 1)
              (ExpApply (ExpVar 2) (ExpVar 3)))
            (ExpVar 3)))))
  , (,) "join"
    (ExpLambda 1
      (ExpApply
        (ExpApply (ExpLit "(>>=)") (ExpVar 1))
        (ExpLambda 5 (ExpVar 5))))
  , (,) "fjoin"
    (ExpLambda 1
      (ExpLambda 2
        (ExpApply (ExpApply (ExpVar 1) (ExpVar  2)) (ExpVar 2))))
  , (,) "zipThingy"
    (ExpLambda 1
      (ExpLambda 2
        (ExpApply
          (ExpApply
            (ExpLit "fmap")
            (ExpLambda 6
              (ExpApply
                (ExpApply (ExpLit "(,)") (ExpVar 6))
                (ExpVar 2))))
          (ExpVar 1))))
  , (,) "stateRun"
    (ExpLambda 1
      (ExpLambda 2
        (ExpLetMatch "State" [4] (ExpVar 1)
          (ExpLetMatch "(,)" [7,8]
            (ExpApply (ExpVar 4) (ExpVar 2))
            (ExpVar 7)))))
  , (,) "fst"
    (ExpLambda 1 (ExpLetMatch "(,)" [3,4] (ExpVar 1) (ExpVar 3)))
  , (,) "snd"
    (ExpLambda 1 (ExpLetMatch "(,)" [3,4] (ExpVar 1) (ExpVar 4)))
  , (,) "quad"
    (ExpLambda 1
      (ExpApply
        (ExpApply
          (ExpLit "(,)")
          (ExpApply (ExpApply (ExpLit "(,)") (ExpVar 1)) (ExpVar 1)))
        (ExpApply (ExpApply (ExpLit "(,)") (ExpVar 1)) (ExpVar 1))))
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
  , (,) "stateBind"
    (ExpLambda 1
      (ExpLambda 2
        (ExpLetMatch "State" [4] (ExpVar 1)
          (ExpApply
            (ExpLit "State")
            (ExpLambda 6
              (ExpLetMatch "(,)" [10,11]
                (ExpApply
                  (ExpVar 4)
                  (ExpVar 6))
                (ExpLetMatch "State" [14]
                  (ExpApply (ExpVar 2) (ExpVar 10))
                  (ExpApply (ExpVar 14) (ExpVar 11)))))))))
  , (,) "dbMaybe"
    (ExpLambda 1
      (ExpApply
        (ExpApply
          (ExpLit "fmap")
          (ExpLambda 5
            (ExpApply
              (ExpApply (ExpLit "(,)") (ExpVar 5))
              (ExpVar 5))))
        (ExpVar 1)))
  ]

checkOutput :: [( String
                , Expression
                , Maybe (Expression, Expression)
                , Maybe (Int, ExferenceStats)
                )]
checkOutput = do
  (iname, allowUnused, typeStr) <- testInput
  (ename, expr)    <- expected
  guard $ iname==ename
  let input = ExferenceInput
                (readConstrainedType defaultContext typeStr)
                bindings
                defaultContext
                allowUnused
  let r = findExpressions input
  let finder :: Int -> [(Expression, ExferenceStats)] -> Maybe (Int, ExferenceStats)
      finder n [] = Nothing
      finder n ((e, s):r) | e==expr = Just (n, s)
                          | otherwise = finder (n+1) r
      bestFound = findSortNExpressions 100 input
      firstAndBest :: Maybe (Expression, Expression)
      firstAndBest = do
        (f,_) <- listToMaybe r
        (b,_) <- listToMaybe bestFound
        return (f,b)
  return (iname, expr, firstAndBest, finder 0 r)

testOutput :: [[(Expression, ExferenceStats)]]
testOutput = map f testInput
  where
    input = ExferenceInput
    f (_, allowUnused, s) = takeFindSortNExpressions 5 10 $ ExferenceInput
                (readConstrainedType defaultContext s)
                bindings
                defaultContext
                allowUnused

testInOut = zip testInput testOutput

printAndStuff = mapM_ f testInOut
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

printStatistics = mapM_ f testInOut
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

printChecks :: IO ()
printChecks = mapM_ helper checkOutput
  where
    helper :: ( String
              , Expression
              , Maybe (Expression, Expression)
              , Maybe (Int, ExferenceStats))
           -> IO ()
    helper (name, _, Just(f,b), Just (0, ExferenceStats n d))
      | f==b = do putStrLn $ printf "%-10s: fine                                  %5d %8.2f" name n d
      | otherwise = do
      putStrLn $ printf "%-10s: expected solution first, but not best!" name
      putStrLn $ "  expected solution: " ++ show f
      putStrLn $ "  best solution:     " ++ show b
    helper (name, e, Just(f,_), Just (i, _)) = do
      putStrLn $ printf "%-10s: expected solution not first, but %d!" name i
      putStrLn $ "  first solution:    " ++ show f
      putStrLn $ "  expected solution: " ++ show e
    helper (name, e, Just(f,b), Nothing) = do
      putStrLn $ printf "%-10s: expected solution not found!" name
      putStrLn $ "  first solution was " ++ show f
      putStrLn $ "  best solution:     " ++ show b
      putStrLn $ "  expected solution: " ++ show e
    helper (name, _, Nothing, _) = do
      putStrLn $ printf "%-10s: no solutions found at all!" name 

-- TODO: remove duplication
pointfree :: String -> IO String
pointfree s = (!!1) <$> lines <$> readProcess "pointfree" ["--verbose", s] ""

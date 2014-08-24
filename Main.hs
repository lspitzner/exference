module Main
  ( main
  )
where



import Type
import ConstrainedType
import Unify
import KnownDict
import Debug.Hood.Observe
import Infression
import TypeClasses
import Expression

import Control.DeepSeq

import System.Process

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( second, (***) )
import Control.Monad ( when, forM_ )



printFindExpression :: String -> String -> IO ()
printFindExpression name typeStr = do
  let f (e, InfressionStats n d) = do
          let str = show e
              doPf = False
          if doPf then do
            pf <- pointfree $ str
            putStrLn $ name ++ " = " ++ pf
                       ++ "    FROM    " ++ name ++ " = " ++ str
                       ++ " (depth " ++ show d ++ ", " ++ show n ++ " steps)"
           else
            putStrLn $ name ++ " = " ++ str
                       ++ " (depth " ++ show d ++ ", " ++ show n ++ " steps)"
  let
    r = take 6 $ findExpressions
      (readConstrainedType defaultContext typeStr)
      bindings
      defaultContext
  when (null r) $ putStrLn $ "no results for "++name++"!"
  mapM_ f r

testInput :: [(String, String)]
testInput = 
  [ (,) "showmap"   "(Show b) => (a -> b) -> List a -> List String"
  , (,) "ffbind"    "(a -> t -> b) -> (t -> a) -> (t -> b)"
  , (,) "join"      "(Monad m) => m (m a) -> m a"
  , (,) "fjoin"     "(t -> (t -> a)) -> t -> a"
  , (,) "zipThingy" "List a -> b -> List (Tuple a b)"
  , (,) "stateRun"  "State a b -> a -> b"
  , (,) "fst"       "Tuple a b -> a"
  , (,) "ffst"      "(a -> Tuple b c) -> a -> b"
  , (,) "snd"       "Tuple a b -> b"
  , (,) "fswap"     "(a -> Tuple b c) -> a -> Tuple c b"
  , (,) "liftBlub"  "Monad m => m a -> m b -> (a -> b -> m c) -> m c"
  , (,) "stateBind" "State s a -> (a -> State s b) -> State s b"
  ]

testOutput :: [[(Expression, InfressionStats)]]
testOutput = map f testInput
  where
    f (_,s) = take 6 $ findExpressions
                (readConstrainedType defaultContext s)
                bindings
                defaultContext

testInOut = zip testInput testOutput

printAndStuff = mapM_ f testInOut
  where
    f ((name, _), []) = putStrLn $ "no results for "++name++"!"
    f ((name, _), results) = mapM_ g results
      where
        g (expr, InfressionStats n d) = do
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
    f ((name, _), [])      = putStrLn ("---")
    f ((name, _), results) =
      let (min, avg, max, n) = getStats results
      in putStrLn $ name ++ " min=" ++ show min
                         ++ " avg=" ++ show avg
                         ++ " max=" ++ show max
                         ++ " n = " ++ show n
    getStats results =
      let steps = map (infression_steps.snd) results
      in ( minimum steps
         --, fromIntegral (sum steps) / fromIntegral (length steps)
         , sum steps `div` length steps
         , maximum steps
         , length steps
         )

main = runO $ do
  --print $ result1
  -- let (DynContext _a b _) = testDynContext
  -- print b
  printAndStuff
  printStatistics
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

result1 = unify (TypeArrow (TypeVar 0) (TypeCons "Blub"))
               (TypeArrow (TypeCons "Foo") (TypeVar 1))

result4 = take 10 $ findExpressions
  -- (readConstrainedType defaultContext "(Show B) => B -> String")
  (readConstrainedType defaultContext "(Show B) => (A -> B) -> List A -> List String")
  -- (readConstrainedType defaultContext "(A -> B) -> List A -> List B")
  bindings
  defaultContext
  
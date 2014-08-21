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

import Control.DeepSeq

import System.Process

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( second, (***) )
import Control.Monad ( when )



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
    r = take 10 $ findExpressions
      (readConstrainedType defaultContext typeStr)
      bindings
      defaultContext
  when (null r) $ putStrLn $ "no results for "++name++"!"
  mapM_ f r

main = runO $ do
  --print $ result1
  -- let (DynContext _a b _) = testDynContext
  -- print b
  --printFindExpression "showmap" "(Show b) => (a -> b) -> List a -> List String"
  --printFindExpression "ffbind" "(a -> t -> b) -> (t -> a) -> (t -> b)"
  --printFindExpression "join" "(Monad m) => m (m a) -> m a"
  --printFindExpression "fjoin" "(t -> (t -> a)) -> t -> a"
  --printFindExpression "zipThingy" "List a -> b -> List (Tuple a b)"
  --printFindExpression "stateRun" "State a b -> a -> b"
  printFindExpression "fst" "Tuple a b -> a"
  --print $ inflateConstraints a b
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
  
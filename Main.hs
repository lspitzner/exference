module Main where



import Type
import ConstrainedType
import Unify
import Match
import KnownDict
import Debug.Hood.Observe
import Control.Arrow ( second )
import Infression
import TypeClasses

import Control.DeepSeq

import System.Process

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( (***) )
import Control.Monad ( when )


main = runO $ do
  --print $ result1
  --print $ result2
  --print $ result3
  let f (n,s) = do
          let str = show s
          pf <- (!!1) <$> lines <$> pointfree str
          putStrLn $ pf ++ "    FROM    " ++ str ++ " (" ++ show n ++ " steps)"
  mapM_ f $!! result4
  when (null result4) $ putStrLn "no results!"
  let (DynContext a b _) = testDynContext
  print $ b
  --print $ inflateConstraints a b
  {-
  print $ constraintMatches testDynContext (badReadVar "y") (read "x")
  print $ constraintMatches testDynContext (badReadVar "x") (read "y")
  print $ constraintMatches testDynContext (badReadVar "w") (read "MyFoo")
  print $ constraintMatches testDynContext (badReadVar "w") (read "MyBar")
  print $ constraintMatches testDynContext (badReadVar "y") (read "MyFoo")
  -}
  print $ isProvable testDynContext [Constraint c_applicative [read "y"]]
  -- putStrLn $ show n ++ " total steps"
  {-
  let t :: HsType
      t = read "m a->( ( a->m b)->( m b))"
  print $ t
  -}

pointfree :: String -> IO String
pointfree s = readProcess "pointfree" ["--verbose", s] ""

result1 = unify (TypeArrow (TypeVar 0) (TypeCons "Blub"))
               (TypeArrow (TypeCons "Foo") (TypeVar 1))

result2 = [(s1, s2, r) | (s1, t1) <- bindings, (s2, t2) <- bindings, r <- inflateUnify t1 t2]

result3 = applyN 1 typeId typeBind

result4 = take 10 $ findExpression
  (readConstrainedType defaultContext "(A -> B) -> List A -> List B")
  bindings
  defaultContext
  
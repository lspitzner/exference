module Main where



import Type
import Unify
import Match
import KnownDict
import Debug.Hood.Observe
import Control.Arrow ( second )
import Infression
import Control.DeepSeq

import System.Process

import Control.Applicative ( (<$>), (<*>) )



main = runO $ do
  print $ result1
  --print $ result2
  print $ result3
  let f s = do
          let str = show s
          pf <- (!!1) <$> lines <$> pointfree str
          putStrLn $ pf ++ "    FROM    " ++ str
  mapM_ f $!! result4
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

result4 = take 10 $ findExpression typeJoin bindings
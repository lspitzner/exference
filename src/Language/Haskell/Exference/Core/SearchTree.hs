{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.Core.SearchTree
  ( SearchTree
  , SearchTreeValue
  , SearchTreeBuilder
  , initialSearchTreeBuilder
  , buildSearchTree
  , filterSearchTreeN
  , filterSearchTreeProcessedN
  , takeSearchTree
  )  
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.Expression

import Data.Tree
import Data.Maybe ( fromMaybe )
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable ( Hashable )



type SearchTreeValue = ( Int         -- total number of children
                       , Int         -- number of processed children
                       , Expression  -- expression
                       , Bool        -- processed
                       )

type SearchTree = Tree SearchTreeValue


type SearchTreeBuilder a = ( [(a, a, Expression)] -- id, parentid, expr,
                           , [a]                  -- processed list
                           )

type SearchTreeBuilderTemp a = ( HM.HashMap a Expression
                               , HM.HashMap a [a]
                               )

buildSearchTree :: forall a
                 . (Eq a, Hashable a)
                => SearchTreeBuilder a
                -> a
                -> SearchTree
buildSearchTree (assocs,processed) root =
  r
    where
      r :: SearchTree
      r = ff pureTree
      isProcessed (_,_,_,x) = x
      ff (Node (x,e) ts)
        | eval <- HS.member x processedSet
        , subtrees <- map ff ts
        = Node ( 1 + length (concatMap flatten ts)
               , if eval || not (null ts)
                   then 1 + length (filter isProcessed
                                   $ concatMap flatten subtrees)
                   else 0
               , e
               , eval)
               subtrees
      processedSet = HS.fromList processed
      pureTree :: Tree (a,Expression)
      pureTree = runReader (unfoldTreeM f root) (mv,mp)
      f :: a -> Reader (SearchTreeBuilderTemp a) ((a,Expression), [a])
      f x = do
        (mValues, mChildren) <- ask
        return $ ((x, mValues HM.! x), fromMaybe [] $ HM.lookup x mChildren)
      mv = HM.fromList $ map (\(i,_,v) -> (i,v)) assocs
      mp = HM.fromListWith (++)
         $ assocs >>= \(i,p,_) -> if i==p then [] else [(p, [i])]

initialSearchTreeBuilder :: a -> Expression -> SearchTreeBuilder a
initialSearchTreeBuilder x e = ([(x,x,e)],[])

-- removes all nodes that have less than n total nodes (incl. self)
-- e.g. if n==2, all nodes without children are removed.
filterSearchTreeN :: Int -> SearchTree -> SearchTree
filterSearchTreeN n (Node d ts) = Node d (ts >>= f)
  where
    f :: SearchTree -> [SearchTree]
    f (Node d'@(k,_,_,_) ts') | n>k = []
                              | otherwise = [Node d' $ ts' >>= f]

-- removes all nodes that have less than n total nodes (incl. self)
-- e.g. if n==2, all nodes without children are removed.
filterSearchTreeProcessedN :: Int -> SearchTree -> SearchTree
filterSearchTreeProcessedN n (Node d ts) = Node d (ts >>= f)
  where
    f :: SearchTree -> [SearchTree]
    f (Node d'@(_,k,_,_) ts') | n>k = []
                              | otherwise = [Node d' $ ts' >>= f]

-- limits depth of tree
takeSearchTree :: Int -> SearchTree -> SearchTree
takeSearchTree 0 (Node d _) = Node d []
takeSearchTree n _ | n<0 = error "takeSearchTree: negative depth"
takeSearchTree n (Node d ts) = Node d [takeSearchTree (n-1) t | t <- ts]

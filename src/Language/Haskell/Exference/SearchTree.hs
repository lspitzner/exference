{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Exference.SearchTree
  ( SearchTree
  , SearchTreeValue
  , SearchTreeBuilder
  , initialSearchTreeBuilder
  , buildSearchTree
  )  
where



import Language.Haskell.Exference.Type
import Language.Haskell.Exference.Expression

import Data.Tree
import Data.Maybe ( fromMaybe )
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable ( Hashable )


type SearchTreeValue = ( Int         -- total number of children;
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
      ff (Node (x,e) ts) = Node ( 1 + length (concatMap flatten ts)
                                , e
                                , HS.member x processedSet)
                                (map ff ts)
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

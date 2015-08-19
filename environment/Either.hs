module Data.Either where



data Either a b = Left a
                | Right b

-- replacable by pattern-matching; causes larger search-space
-- either :: (a->c) -> (b->c) -> Either a b -> c
partitionEithers :: [Either a b] -> ([a], [b]) 

instance Bifunctor Either
instance Control.Monad.Monad (Either e)
instance Data.Functor.Functor (Either a)
instance Control.Monad.MonadFix (Either e)
instance Control.Applicative.Applicative (Either e)
instance Data.Foldable.Foldable (Either a)
instance Data.Traversable.Travesable (Either a)
instance GHC.Generics.Generic1 (Either a)
instance (Data.Eq.Eq a, Data.Eq.Eq b) => Data.Eq.Eq (Either a b)
instance (Data.Ord.Ord a, Data.Ord.Ord b) => Data.Ord.Ord (Either a b)
instance (Text.Read.Read a, Text.Read.Read b) => Text.Read.Read (Either a b)
instance (Text.Show.Show a, Text.Show.Show b) => Text.Show.Show (Either a b)
instance GHC.Generics.Generic (Either a b)

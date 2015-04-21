module Data.Either where



data Either a b = Left a
                | Right b

either :: (a->c) -> (b->c) -> Either a b -> c
partitionEithers :: [Either a b] -> ([a], [b]) 

instance Bifunctor Either
instance Control.Monad.Monad (Either e)
instance Data.Functor.Functor (Either a)
instance Control.Monad.MonadFix (Either e)
instance Control.Applicative.Applicative (Either e)
instance Data.Foldable.Foldable (Either a)
instance Data.Traversable.Travesable (Either a)
instance Generic1 (Either a)
instance (Eq a, Eq b) => Eq (Either a b)
instance (Data a, Data b) => Data (Either a b)
instance (Ord a, Ord b) => Ord (Either a b)
instance (Read a, Read b) => Read (Either a b)
instance (Show a, Show b) => Show (Either a b)
instance Generic (Either a b)

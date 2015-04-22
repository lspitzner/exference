module Data.Maybe where



data Maybe a = Just a
             | Nothing

maybe :: b -> (a -> b) -> Maybe a -> b
fromMaybe :: a -> Maybe a -> a

instance Control.Monad.Monad Maybe
instance Data.Functor.Functor Maybe
instance Control.Monad.MonadFix Maybe
instance Control.Applicative.Applicative Maybe
instance Data.Foldable.Foldable Maybe
instance Data.Traversable.Travesable Maybe
instance GHC.Generics.Generic1 Maybe
instance Control.Monad.MonadPlus Maybe
instance Control.Applicative.Alternative Maybe
instance Data.Eq.Eq a => Data.Eq.Eq (Maybe a)
instance Data.Data.Data a => Data.Data.Data (Maybe a)
instance Data.Ord.Ord a => Data.Ord.Ord (Maybe a)
instance Text.Read.Read a => Text.Read.Read (Maybe a)
instance Text.Show.Show a => Text.Show.Show (Maybe a)
instance GHC.Generics.Generic (Maybe a)
instance Data.Monoid.Monoid a => Data.Monoid.Monoid (Maybe a)

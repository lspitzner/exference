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
instance Generic1 Maybe
instance Control.Monad.MonadPlus Maybe
instance Control.Applicative.Alternative Maybe
instance Eq a => Eq (Maybe a)
instance Data a => Data (Maybe a)
instance Ord a => Ord (Maybe a)
instance Read a => Read (Maybe a)
instance Text.Show.Show a => Text.Show.Show (Maybe a)
instance Generic (Maybe a)
instance Monoid a => Monoid (Maybe a)

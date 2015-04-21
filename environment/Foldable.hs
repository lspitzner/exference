module Data.Foldable where



class Foldable t where
  fold :: Data.Monoid.Monoid m => t m -> m
  foldMap :: Data.Monoid.Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b

asum :: (Foldable t, Control.Applicative.Alternative f)
     => t (f a)
     -> f a 

instance Foldable []
instance Foldable Data.Maybe.Maybe
instance Foldable Identity
instance Foldable (Data.Either.Either a)
instance Foldable ((,) a)
-- instance Foldable (Proxy *)
instance Foldable (Const m)

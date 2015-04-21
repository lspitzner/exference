module Data.Traversable where



class (Data.Functor.Functor t, Data.Foldable.Foldable t) => Traversable t where
  traverse :: Control.Applicative.Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Control.Applicative.Applicative f => t (f a) -> f (t a)
  -- mapM :: Control.Monad.Monad m => (a -> m b) -> t a -> m (t b)
  -- sequence :: Control.Monad.Monad m => t (m a) -> m (t a)

instance Traversable []
instance Traversable Data.Maybe.Maybe
instance Traversable Identity
-- instance Traversable (Data.Either.Either a)  -- cause "unused" problems
-- instance Traversable ((,) a)     -- cause "unused" problems
-- instance Traversable (Proxy *)
instance Traversable (Const m)

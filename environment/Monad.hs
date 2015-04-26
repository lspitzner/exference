module Control.Monad where




class Control.Applicative.Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

class (Control.Applicative.Alternative m, Monad m) => MonadPlus m where
  -- mzero :: m a -- this is a critical case: too generic, yet we might need it
                  -- at times
  -- more specific than (<|>)
  -- mplus :: m a -> m a -> m a

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
join :: Monad m => m (m a) -> m a
msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
mfilter :: MonadPlus m => (a -> Data.Bool.Bool) -> m a -> m a 
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c] 
foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b 


instance Monad []
instance Monad IO
instance Monad Data.Maybe.Maybe
instance Monad ReadP
instance Monad ReadPrec
instance Monad STM
-- instance Monad ((->) r)
instance Monad (Data.Either.Either e)
instance Monad (ST s)
-- instance Monad (Proxy *)
instance ArrowApply a => Monad (ArrowMonad a)
instance Monad (ST s)
instance Monad m => Monad (WrappedMonad m)

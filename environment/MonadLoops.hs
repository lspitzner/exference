module Control.Monad.Loops where



-- whileM :: Control.Monad.Monad m => m Data.Bool.Bool -> m a -> m [a]
whileM' :: (Control.Monad.Monad m, Control.Monad.MonadPlus f) => m Data.Bool.Bool -> m a -> m (f a) 
iterateWhile :: Control.Monad.Monad m => (a -> Bool) -> m a -> m a 

-- this is evil, prove-wise
iterateM_ :: Control.Monad.Monad m => (a -> m a) -> a -> m b

-- untilM :: Control.Monad.Monad m => m a -> m Data.Bool.Bool -> m [a]
untilM' :: (Control.Monad.Monad m, Control.Monad.MonadPlus f) => m a -> m Data.Bool.Bool -> m (f a)

-- this is evil, in it discards results
-- iterateWhile :: Control.Monad.Monad m => (a -> Data.Bool.Bool) -> m a -> m a

concatM :: Control.Monad.Monad m => [a -> m a] -> a -> m a 

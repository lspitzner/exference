module Control.Monad.IO.Class where



class Control.Monad.Monad m => MonadIO m where
  liftIO :: System.IO.IO a -> m a 

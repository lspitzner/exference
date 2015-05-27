module Control.Monad.Trans.Class where



class MonadTrans t where
    lift :: (Control.Monad.Monad m) => m a -> t m a

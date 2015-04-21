module Control.Monad.State where



newtype State s a = State (s -> (a, s))
newtype StateT s m a = StateT (s -> m (a, s))

state :: (Control.Monad.Monad m)
      => (s -> (a, s))  -- ^pure state transformer
      -> StateT s m a   -- ^equivalent state-passing computation

get :: Control.Monad.Monad m => StateT s m s 
put :: Control.Monad.Monad m => s -> StateT s m () 

instance Control.Monad.MonadTrans (StateT s)
instance Control.Monad.Monad m => Control.Monad.Monad (StateT s m)
instance Data.Functor.Functor m => Data.Functor.Functor (StateT s m)
instance Control.Monad.MonadFix m => MonadFix (StateT s m)
instance (Data.Functor.Functor m, Control.Monad.Monad m) => Control.Applicative.Applicative (StateT s m)
instance (Data.Functor.Functor m, Control.Monad.MonadPlus m) => Control.Applicative.Alternative (StateT s m)
instance Control.Monad.MonadPlus m => Control.Monad.MonadPlus (StateT s m)
instance Control.Monad.MonadIO m => MonadIO (StateT s m)

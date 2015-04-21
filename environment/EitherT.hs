module Control.Monad.Trans.Either where



newtype EitherT e m a = EitherT (m (Data.Either.Either e a))

runEitherT :: EitherT e m a -> m (Data.Either.Either e a)

instance Control.Monad.Monad m => Control.Monad.Monad (EitherT e m)

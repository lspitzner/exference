module Control.Monad.Trans.Cont where



newtype Cont r a = Cont ((a -> r) -> r)

-- runCont :: Cont r a -> (a -> r) -> r

-- instance Control.Monad.Monad (Cont r)

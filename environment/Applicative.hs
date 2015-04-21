module Control.Applicative where



class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a->b) -> f a -> f b

class Applicative f => Alternative f where
  -- empty :: f a
  (<|>) :: f a -> f a -> f a
  -- some :: f a -> f [a]
  -- many :: f a -> f [a] 

liftA2 :: Applicative f => (a1 -> a2 -> r) -> f a1 -> f a2 -> f r
liftA3 :: Applicative f => (a1 -> a2 -> a3 -> r) -> f a1 -> f a2 -> f a3 -> f r 

instance Applicative []
instance Applicative IO
instance Applicative Data.Maybe.Maybe
instance Applicative ReadP
instance Applicative ReadPrec
instance Applicative STM
instance Applicative ZipList
-- instance Applicative ((->) a)
instance Applicative (Data.Either.Either e)
instance Monoid a => Applicative ((,) a)
instance Applicative (ST s)
-- instance Applicative (Proxy *)
instance Arrow a => Applicative (ArrowMonad a)
instance Applicative (ST s)
instance Control.Monad.Monad m => Applicative (WrappedMonad m)
instance Monoid m => Applicative (Const m)
instance Arrow a => Applicative (WrappedArrow a b)
-- instance Typeable ((* -> *) -> Constraint) Applicative

instance Alternative []
instance Alternative Data.Maybe.Maybe
instance Alternative ReadP
instance Alternative ReadPrec
instance Alternative STM
instance ArrowPlus a => Alternative (ArrowMonad a)
instance Control.Monad.MonadPlus m => Alternative (WrappedMonad m)
-- instance Alternative f => Alternative (Alt * f)
instance (ArrowZero a, ArrowPlus a) => Alternative (WrappedArrow a b)

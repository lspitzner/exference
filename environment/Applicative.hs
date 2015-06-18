module Control.Applicative where



class Data.Functor.Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a->b) -> f a -> f b

class Applicative f => Alternative f where
  -- empty :: f a
  (<|>) :: f a -> f a -> f a
  -- some :: f a -> f [a]
  -- many :: f a -> f [a] 

liftA2 :: Applicative f => (a1 -> a2 -> r) -> f a1 -> f a2 -> f r
liftA3 :: Applicative f => (a1 -> a2 -> a3 -> r) -> f a1 -> f a2 -> f a3 -> f r 

instance Data.Monoid.Monoid a => Applicative ((,) a)
instance Arrow a => Applicative (ArrowMonad a)
instance Control.Monad.Monad m => Applicative (Control.Monad.WrappedMonad m)
instance Data.Monoid.Monoid m => Applicative (Const m)
instance Arrow a => Applicative (WrappedArrow a b)
-- instance Typeable ((* -> *) -> Constraint) Applicative

instance Alternative []
instance ArrowPlus a => Alternative (ArrowMonad a)
instance Control.Monad.MonadPlus m => Alternative (WrappedMonad m)
-- instance Alternative f => Alternative (Alt * f)
instance (ArrowZero a, ArrowPlus a) => Alternative (WrappedArrow a b)

data ZipList a

instance Data.Functor.Functor ZipList
instance Control.Applicative.Applicative ZipList
instance GHC.Generics.Generic1 ZipList
instance Data.Eq.Eq a => Data.Eq.Eq (ZipList a)
instance Data.Ord.Ord a => Data.Ord.Ord (ZipList a)
instance Text.Read.Read a => Text.Read.Read (ZipList a)
instance Text.Show.Show a => Text.Show.Show (ZipList a)
instance GHC.Generics.Generic (ZipList a)

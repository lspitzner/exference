module Data.Functor where



class Functor f where
  fmap :: (a->b) -> f a -> f b
  -- (<$) :: a -> f b -> f a

instance Functor []
instance Functor IO
instance Functor Data.Maybe.Maybe
instance Functor ReadP
instance Functor ReadPrec
instance Functor STM
instance Functor Handler
instance Functor ZipList
instance Functor ArgDescr
instance Functor OptDescr
instance Functor ArgOrder
-- instance Functor ((->) r)
instance Functor (Data.Either.Either a)
instance Functor ((,) a)
instance Functor (ST s)
-- instance Functor (Proxy *)
instance Arrow a => Functor (ArrowMonad a)
instance Functor (ST s)
instance Control.Monad.Monad m => Functor (WrappedMonad m)
instance Functor (Const m)
instance Arrow a => Functor (WrappedArrow a b)

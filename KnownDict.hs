module KnownDict where



import Type
import ConstrainedType
import TypeClasses

import Control.Arrow ( second )



typeReturn, typeUnsafe, typeBind, typeJoin, typeId :: HsType
typeId     = read "a -> a"
typeReturn = read "a -> m a"
typeUnsafe = read "a -> b"
typeBind   = read "m a -> (a -> m b) -> m b"
typeJoin   = read "m (m a) -> m a"

type Binding = (String, HsConstrainedType)

bindings :: [Binding]
bindings =
  [ ("fmap", readConstrainedType defaultContext "(Functor f) => (a -> b) -> f a -> f b")
  , ("(*)", readConstrainedType defaultContext "(Applicative f) => f (a->b) -> f a -> f b")
  , ("pure", readConstrainedType defaultContext "(Applicative f) => a -> f a")
  , ("(>>=)", readConstrainedType defaultContext "(Monad m) => m a -> (a -> m b) -> m b")
  -- , ("map", readConstrainedType defaultContext "(a->b) -> List a -> List b")
  , ("show", readConstrainedType defaultContext "(Show a) => a -> String")
  ]
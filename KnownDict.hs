module KnownDict where



import Type

import Control.Arrow ( second )



typeReturn, typeUnsafe, typeBind, typeJoin, typeId :: HsType
typeId     = read "a -> a"
typeReturn = read "a -> m a"
typeUnsafe = read "a -> b"
typeBind   = read "m a -> (a -> m b) -> m b"
typeJoin   = read "m (m a) -> m a"
type Binding = (String, HsType)

bindings :: [Binding]
bindings =
  [ ("return", read "a -> m a")
  --, ("unsafe", read "a -> b")
  , ("(>>=)", read "m a -> (a -> m b) -> m b")
  ]
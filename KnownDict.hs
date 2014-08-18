module KnownDict
  ( bindings
  , emptyContext
  , defaultContext
  , testDynContext
  , typeId
  , typeReturn
  , typeUnsafe
  , typeBind
  , typeJoin
  )
where



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
  [ ("fmap",  readConstrainedType defaultContext
              "(Functor f) => (a -> b) -> f a -> f b")
  , ("(*)",   readConstrainedType defaultContext
              "(Applicative f) => f (a->b) -> f a -> f b")
  , ("pure",  readConstrainedType defaultContext
              "(Applicative f) => a -> f a")
  , ("(>>=)", readConstrainedType defaultContext
              "(Monad m) => m a -> (a -> m b) -> m b")
  -- , ("map",  readConstrainedType defaultContext
  --            "(a->b) -> List a -> List b")
  , ("show",  readConstrainedType defaultContext
              "(Show a) => a -> String")
  ]

emptyContext :: StaticContext
emptyContext = StaticContext {
  context_tclasses = [],
  context_instances = []
}

defaultContext :: StaticContext
defaultContext = StaticContext {
  context_tclasses = [c_show, c_functor, c_applicative, c_monad],
  context_instances = [list_show, list_functor, list_applicative, list_monad]
  --context_redirects = M.Map TVarId TVarId
}

c_show           = HsTypeClass "Show" [badReadVar "a"] []
c_functor        = HsTypeClass "Functor" [badReadVar "f"] []
c_applicative    = HsTypeClass "Applicative" [badReadVar "f"]
                                             [Constraint c_functor [read "f"]]
c_monad          = HsTypeClass "Monad" [badReadVar "m"]
                                       [Constraint c_applicative [read "m"]]
c_monadState     = HsTypeClass
                     "MonadState"
                     [badReadVar "s", badReadVar "m"]
                     [Constraint c_monad [read "m"]]
list_show        = HsInstance [Constraint c_show [read "a"]] c_show [read "List a"]
list_functor     = HsInstance [] c_functor     [read "List"]
list_applicative = HsInstance [] c_applicative [read "List"]
list_monad       = HsInstance [] c_monad       [read "List"]

testDynContext = mkDynContext defaultContext
    [ Constraint c_show [read "v"]
    , Constraint c_show [read "w"]
    , Constraint c_functor [read "x"]
    , Constraint c_monad   [read "y"]
    , Constraint c_monadState [read "s", read "z"]
    , Constraint c_show [read "MyFoo"]
    , Constraint c_show [read "B"]
    ]

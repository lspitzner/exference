{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

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

type Binding = (String, Float, HsConstrainedType)

toBindings :: [(String, Float, String)] -> [(String, Float, HsConstrainedType)]
toBindings = map (\(a,b,c) ->
                   (a,b,readConstrainedType defaultContext c))

-- function, penalty for using that function, type
-- value ignored for pattern-matches
bindings :: [Binding]
bindings = toBindings
  [ ("fmap",     3.0, "(Functor f) => (a -> b) -> f a -> f b")
  , ("(*)",      3.0, "(Applicative f) => f (a->b) -> f a -> f b")
  , ("pure",     3.0, "(Applicative f) => a -> f a")
  , ("(>>=)",    0.0, "(Monad m) => m a -> (a -> m b) -> m b")
  , ("(>>)",     0.0, "Monad m => m a -> m b -> m b")
  , ("show",     3.0, "(Show a) => a -> String")
  , ("(,)",      5.0, "a -> b -> Tuple a b")
  , ("zip",      0.0, "List a -> List b -> List (Tuple a b)")
  , ("repeat",   5.0, "a -> List a")
  , ("foldr",    3.0, "(a -> b -> b) -> b -> List a -> b")
  , ("()",       9.9, "Unit")
  , ("State",    0.0, "(s -> Tuple a s) -> State s a")
  , ("empty",    9.9, "List a")
  , ("(:)",      4.0, "a -> List a -> List a")
  , ("(,)",      0.0, "Tuple a b -> INFPATTERN a b")
  , ("State",    0.0, "State s a -> INFPATTERN (s -> Tuple a s)")
  , ("Just",     0.0, "a -> Maybe a")
  ]

emptyContext :: StaticContext
emptyContext = StaticContext {
  context_tclasses = [],
  context_instances = []
}

defaultContext :: StaticContext
defaultContext = StaticContext {
  context_tclasses = [c_show, c_functor, c_applicative, c_monad],
  context_instances = [ list_show, list_functor, list_applicative, list_monad
                      , maybe_functor, maybe_applicative, maybe_monad]
  --context_redirects = M.Map TVarId TVarId
}

c_show            = HsTypeClass "Show" [badReadVar "a"] []
c_functor         = HsTypeClass "Functor" [badReadVar "f"] []
c_applicative     = HsTypeClass "Applicative" [badReadVar "f"]
                                              [Constraint c_functor [read "f"]]
c_monad           = HsTypeClass "Monad" [badReadVar "m"]
                                        [Constraint c_applicative [read "m"]]
c_monadState      = HsTypeClass
                      "MonadState"
                      [badReadVar "s", badReadVar "m"]
                      [Constraint c_monad [read "m"]]
list_show         = HsInstance [Constraint c_show [read "a"]] c_show [read "List a"]
list_functor      = HsInstance [] c_functor     [read "List"]
list_applicative  = HsInstance [] c_applicative [read "List"]
list_monad        = HsInstance [] c_monad       [read "List"]
maybe_functor     = HsInstance [] c_functor     [read "Maybe"]
maybe_applicative = HsInstance [] c_functor     [read "Maybe"]
maybe_monad       = HsInstance [] c_functor     [read "Maybe"]

testDynContext = mkDynContext defaultContext
    [ Constraint c_show [read "v"]
    , Constraint c_show [read "w"]
    , Constraint c_functor [read "x"]
    , Constraint c_monad   [read "y"]
    , Constraint c_monadState [read "s", read "z"]
    , Constraint c_show [read "MyFoo"]
    , Constraint c_show [read "B"]
    ]

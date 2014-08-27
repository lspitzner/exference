{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE PatternGuards #-}

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

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Control.Arrow ( second )

import Debug.Hood.Observe



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
  [
  -- functor
    ("fmap",     1.0, "(Functor f) => (a -> b) -> f a -> f b")
  -- applicative
  , ("pure",     3.0, "(Applicative f) => a -> f a")
  , ("(*)",      3.0, "(Applicative f) => f (a->b) -> f a -> f b")
  -- monad
  , ("(>>=)",    0.0, "(Monad m) => m a -> (a -> m b) -> m b")
  , ("(>>)",     8.0, "Monad m => m a -> m b -> m b")
  -- show
  , ("show",     3.0, "(Show a) => a -> String")
  -- eq
  , ("(==)",     4.0, "Eq a => a -> a -> Bool")
  , ("(/=)",     10.0, "Eq a => a -> a -> Bool") -- bad, cause same sig as (==)..
  -- num
  , ("fromInteger", 9.9, "Num a => Integer -> a")
  -- real
  , ("toRational", 9.9, "Real a => a -> Rational")
  -- integral
  , ("toInteger", 9.9, "Integral a => a -> Integer")
  -- fractional
  , ("fromRational", 9.9, "Fractional a => Rational -> a")
  -- realfrac
  , ("truncate", 9.9, "RealFrac a, Integral b => a -> b")
  , ("round"   , 9.9, "RealFrac a, Integral b => a -> b")
  , ("ceiling" , 9.9, "RealFrac a, Integral b => a -> b")
  , ("floor"   , 9.9, "RealFrac a, Integral b => a -> b")
  -- other
  , ("(,)",      0.0, "a -> b -> Tuple a b")
  , ("zip",      0.0, "List a -> List b -> List (Tuple a b)")
  , ("repeat",   5.0, "a -> List a")
  , ("foldr",    0.0, "(a -> b -> b) -> b -> List a -> b")
  --, ("foldr0",   0.0,  "(a -> List a -> List a) -> a -> List a")
  , ("()",       9.9, "Unit")
  , ("State",    3.0, "(s -> Tuple a s) -> State s a")
  , ("[]",       40.0, "List a")
  , ("(:)",      4.0, "a -> List a -> List a")
  , ("(,)",      0.0, "Tuple a b -> INFPATTERN a b")
  , ("State",    0.0, "State s a -> INFPATTERN (s -> Tuple a s)")
  , ("Just",     5.0, "a -> Maybe a")
  , ("sequence", 3.0, "Monad m => List (m a) -> m (List a)")
  ]

emptyContext :: StaticContext
emptyContext = StaticContext {
  context_tclasses = [],
  context_instances = []
}

defaultContext :: StaticContext
defaultContext = StaticContext {
  context_tclasses = [ c_show
                     , c_functor, c_applicative, c_monad
                     , c_eq, c_ord
                     , c_enum
                     , c_num, c_real, c_integral
                     , c_fractional, c_realfrac, c_floating, c_realfloat],
  context_instances = inflateInstances
                     $ [ list_show, list_monad, maybe_monad, maybe_show, tuple_show]
                    ++ integral_instances
                    ++ realfloat_instances
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
c_eq              = HsTypeClass
                      "Eq"
                      [badReadVar "a"]
                      []
c_num             = HsTypeClass
                      "Num"
                      [badReadVar "a"]
                      [ Constraint c_show [read "a"]
                      , Constraint c_eq [read "a"]]
c_ord             = HsTypeClass
                      "Ord"
                      [badReadVar "a"]
                      [Constraint c_eq [read "a"]]
c_real            = HsTypeClass
                      "Real"
                      [badReadVar "a"]
                      [ Constraint c_ord [read "a"]
                      , Constraint c_num [read "a"]]
c_fractional      = HsTypeClass
                      "Fractional"
                      [badReadVar "a"]
                      [Constraint c_num [read "a"]]
c_enum            = HsTypeClass
                      "Enum"
                      [badReadVar "a"]
                      []
c_integral        = HsTypeClass
                      "Integral"
                      [badReadVar "a"]
                      [ Constraint c_real [read "a"]
                      , Constraint c_enum [read "a"]]
c_realfrac        = HsTypeClass
                      "RealFrac"
                      [badReadVar "a"]
                      [ Constraint c_real [read "a"]
                      , Constraint c_fractional [read "a"]]
c_floating        = HsTypeClass
                      "Floating"
                      [badReadVar "a"]
                      [Constraint c_fractional [read "a"]]
c_realfloat       = HsTypeClass
                      "RealFloat"
                      [badReadVar "a"]
                      [ Constraint c_realfrac [read "a"]
                      , Constraint c_floating [read "a"]]

list_show         = HsInstance [Constraint c_show [read "a"]] c_show [read "List a"]
--list_functor      = HsInstance [] c_functor     [read "List"]
--list_applicative  = HsInstance [] c_applicative [read "List"]
list_monad        = HsInstance [] c_monad       [read "List"]
--maybe_functor     = HsInstance [] c_functor     [read "Maybe"]
--maybe_applicative = HsInstance [] c_functor     [read "Maybe"]
maybe_monad       = HsInstance [] c_functor     [read "Maybe"]
maybe_show        = HsInstance [Constraint c_show [read "a"]] c_show [read "Maybe a"]
tuple_show        = HsInstance [Constraint c_show [read "a"]
                               ,Constraint c_show [read "b"]] c_show [read "Tuple a b"]
integral_instances  = mkInstances c_integral ["Int", "Integer"]
realfloat_instances = mkInstances c_realfloat ["Float", "Double"]


mkInstances :: HsTypeClass -> [String] -> [HsInstance]
mkInstances tc strs = map f strs
  where
    f s = HsInstance [] tc [read s]

testDynContext = mkDynContext defaultContext
    [ Constraint c_show [read "v"]
    , Constraint c_show [read "w"]
    , Constraint c_functor [read "x"]
    , Constraint c_monad   [read "y"]
    , Constraint c_monadState [read "s", read "z"]
    , Constraint c_show [read "MyFoo"]
    , Constraint c_show [read "B"]
    ]

inflateInstances :: [HsInstance] -> [HsInstance]
inflateInstances is = S.toList $ S.unions $ map (S.fromList . f) is
  where
    f :: HsInstance -> [HsInstance]
    f i@(HsInstance iconstrs tclass iparams)
      | (HsTypeClass _ tparams tconstrs) <- tclass
      , substs <- M.fromList $ zip tparams iparams
      = let 
          g :: Constraint -> HsInstance
          g (Constraint ctclass cparams) =
            HsInstance iconstrs ctclass $ map (applySubsts substs) cparams
        in i : concatMap (f.g) tconstrs

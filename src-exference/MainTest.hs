{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}

module MainTest
  ( printAndStuff
  , printCheckExpectedResults
  , printStatistics
  , printMaxUsage
#if BUILD_SEARCH_TREE
  , printSearchTree
#endif
  , filterBindingsSimple -- TODO: refactor/move this
  )
where



import Language.Haskell.Exference.Core ( ExferenceHeuristicsConfig(..)
                                       , findExpressionsWithStats
                                       , ExferenceChunkElement(..)
                                       )
import Language.Haskell.Exference
import Language.Haskell.Exference.ExpressionToHaskellSrc
import Language.Haskell.Exference.BindingsFromHaskellSrc
import Language.Haskell.Exference.ClassEnvFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.Core.FunctionBinding

import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.ExpressionSimplify
import Language.Haskell.Exference.Core.ExferenceStats
import Language.Haskell.Exference.Core.SearchTree

import Control.DeepSeq

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( second, (***) )
import Control.Monad ( when, forM_, guard, forM, mplus, mzero )
import Data.Functor.Identity ( Identity, runIdentity )
import Data.List ( sortBy, find, intercalate, maximumBy )
import Data.Ord ( comparing )
import Text.Printf
import Data.Maybe ( listToMaybe, fromMaybe, maybeToList, catMaybes )
import Data.Either ( lefts, rights )
import Control.Monad.Writer.Strict
import qualified Data.Map as M
import qualified Data.IntMap as IntMap
import Data.Tree ( drawTree )
import Control.Monad.Trans.Maybe ( MaybeT (..) )
import Data.Foldable ( asum )

import Control.Monad.Trans.MultiRWS
import Data.HList.ContainsType

import Language.Haskell.Exts.Syntax ( Module(..), Decl(..), ModuleName(..) )
import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , parseModule
                                    , ParseResult (..)
                                    , ParseMode (..) )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )

-- import Data.PPrint

-- import Debug.Hood.Observe
import Debug.Trace



checkData :: [(String, Bool, Bool, String, [String], [String])]
checkData =
  [ (,,,,,) "showmap"    False False "(Text.Show.Show b) => (a -> b) -> [a] -> [String]"
                                     ["\\f1 -> Data.Functor.fmap (\\g -> Text.Show.show (f1 g))"
                                     ,"\\f1 -> Data.Functor.fmap (((.) Text.Show.show) f1)"
                                     ,"\\f1 -> (\\c -> ((Control.Monad.>>=) c) (\\g -> Control.Applicative.pure (Text.Show.show (f1 g))))"]
                                     []
  , (,,,,,) "ffbind"     False False "(a -> t -> b) -> (t -> a) -> (t -> b)"
                                     ["\\f1 -> (\\f2 -> (\\c -> (f1 (f2 c)) c))"]
                                     []
  , (,,,,,) "join"       False False "(Monad m) => m (m a) -> m a"
                                     ["\\a -> ((Control.Monad.>>=) a) (\\f -> f)"
                                     ,"\\a -> ((Control.Monad.>>=) a) id"
                                     ]
                                     ["join"]
  , (,,,,,) "fjoin"      False False "(t -> (t -> a)) -> t -> a"
                                     ["\\f1 -> (\\b -> (f1 b) b)"]
                                     []
  , (,,,,,) "zipThingy"  False False "[a] -> b -> [(a, b)]"
                                     ["\\as -> (\\b -> ((Data.Functor.fmap (\\g -> ((,) g) b)) as)"
                                     ,"\\as -> (\\b -> (Data.List.zip as) (Control.Applicative.pure b))"
                                     ,"\\as -> ((.) (Data.List.zip as)) Control.Applicative.pure"
                                     ]
                                     []
  , (,,,,,) "pmatch"     False True  "Data.Maybe.Maybe a -> a -> a"
                                     ["\\m1 -> (\\b -> ((Data.Maybe.maybe b) (\\h -> h)) m1)"
                                     ,"\\m1 -> (\\b -> case m1 of { Data.Maybe.Just d -> d; Data.Maybe.Nothing  -> b })"]
                                     []
  --, (,,,,,) "pmatch2"    False True  "Tuple2 (Either a b) c -> Tuple2 (Maybe (Tuple2 a c)) (Maybe (Tuple2 b c))"
  --                                  []
  --                                   []
  , (,,,,,) "stateRun"   True  False "Control.Monad.State.State a b -> a -> b"
                                     ["\\s1 -> (\\b -> let (Control.Monad.State.State f4) = s1 in let ((,) g h) = f4 b in g)"]
                                     []
  , (,,,,,) "fst"        True  False "(a, b) -> a"
                                     ["\\a -> let ((,) c d) = a in c"]
                                     []
  --, (,,,,,) "ffst"       True False  "(a -> Tuple b c) -> a -> b"
  , (,,,,,) "snd"        True  False "(a, b) -> b"
                                     ["\\a -> let ((,) c d) = a in d"]
                                     []
  , (,,,,,) "quad"       False False "a -> ((a, a), (a, a))"
                                     ["\\a -> ((,) (((,) a) a)) (((,) a) a)"]
                                     []
  -- , (,,,,,) "fswap"     False False  "(a -> Tuple b c) -> a -> Tuple c b"
  , (,,,,,) "liftBlub"   False False "Monad m => m a -> m b -> (a -> b -> m c) -> m c"
                                     ["\\a -> (\\b -> (\\f3 -> ((Control.Monad.>>=) a) (\\g -> ((Control.Monad.>>=) b) (f3 g))))"
                                     ,"\\a -> (\\b -> (\\f3 -> ((Control.Monad.>>=) b) (\\g -> ((Control.Monad.>>=) a) (\\k -> (f3 k) g))))"]
                                     []
  , (,,,,,) "stateBind"  False False "Control.Monad.State.State s a -> (a -> Control.Monad.State.State s b) -> Control.Monad.State.State s b"
                                     ["\\s1 -> (\\f2 -> let (Control.Monad.State.State f4) = s1 in Control.Monad.State.State (\\f -> let ((,) j k) = f4 f in let (Control.Monad.State.State f14) = f2 j in f14 k))"]
                                     []
  , (,,,,,) "dbMaybe"    False False "Data.Maybe.Maybe a -> Data.Maybe.Maybe (a, a)"
                                     ["Data.Functor.fmap (\\e -> ((,) e) e)"
                                     ,"\\b -> ((Control.Applicative.liftA2 (\\g -> (\\h -> ((,) h) g))) b) b"
                                     ,"\\b -> ((Control.Monad.>>=) b) (\\f -> Control.Applicative.pure (((,) f) f))"]
                                     []
  , (,,,,,) "tupleShow"  False False "(Text.Show.Show a, Text.Show.Show b) => (a, b) -> String"
                                     ["Text.Show.show"
                                     ,"\\a -> let ((,) c d) = a in Text.Show.show (((,) c) d)"]
                                     []
  , (,,,,,) "FloatToInt" False False "Float -> Int"
                                     [ "Prelude.round"
                                     , "Prelude.floor"
                                     , "Prelude.truncate"
                                     , "Prelude.ceiling"
                                     ]
                                     []
  , (,,,,,) "FloatToIntL" False False "[Float] -> [Int]"
                                     ["Data.Functor.fmap Prelude.round"
                                     ,"Data.Functor.fmap Prelude.floor"
                                     ,"Data.Functor.fmap Prelude.ceiling"
                                     ,"Data.Functor.fmap Prelude.truncate"
                                     ,"\\b -> ((>>=) b) (\\f -> Control.Applicative.pure (Prelude.truncate f))" -- this is kind of ugly
                                     ,"((.) (Data.Functor.fmap Data.Char.ord)) Text.Show.show" -- this is not the solution we really want .. :/
                                     ]
                                     []
  , (,,,,,) "longApp"    False False "a -> b -> c -> (a -> b -> d) -> (a -> c -> e) -> (b -> c -> f) -> (d -> e -> f -> g) -> g"
                                     ["\\a -> (\\b -> (\\c -> (\\f4 -> (\\f5 -> (\\f6 -> (\\f7 -> ((f7 ((f4 a) b)) ((f5 a) c)) ((f6 b) c)))))))"]
                                     []
  , (,,,,,) "liftSBlub"  False False "(Monad m, Monad n) => ([a] -> b -> c) -> m [n a] -> m (n b) -> m (n c)"
                                     ["\\f1 -> Control.Applicative.liftA2 (\\i -> (\\j -> ((Control.Monad.>>=) (Data.Traversable.sequenceA i)) (\\o -> ((Control.Monad.>>=) j) (\\s -> Control.Applicative.pure ((f1 o) s)))))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Applicative.liftA2 (\\i -> (\\j -> ((Control.Monad.>>=) (Data.Traversable.sequenceA j)) (\\o -> ((Control.Monad.>>=) i) (\\s -> Control.Applicative.pure ((f1 o) s)))))) c) b))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) b) (\\gs -> (Data.Functor.fmap (\\k -> ((Control.Monad.>>=) (Data.Traversable.sequenceA gs)) (\\p -> (Data.Functor.fmap (f1 p)) k))) c)))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) c) (\\g -> (Data.Functor.fmap (\\ks -> ((Control.Monad.>>=) (Data.Traversable.sequenceA ks)) (\\p -> (Data.Functor.fmap (f1 p)) g))) b)))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) c) (\\g -> (Data.Functor.fmap (\\ks -> ((Control.Monad.>>=) g) (\\p -> (Data.Functor.fmap (\\t -> (f1 t) p)) ((Data.Traversable.mapM (\\z -> z)) ks)))) b)))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) c) (\\g -> ((Control.Monad.>>=) b) (\\ks -> Control.Applicative.pure (((Control.Monad.>>=) g) (\\p -> (Data.Functor.fmap (\\u -> (f1 u) p)) ((Data.Traversable.mapM (\\t0 -> t0)) ks)))))))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) b) (\\gs -> ((Control.Monad.>>=) c) (\\k -> Control.Applicative.pure (((Control.Monad.>>=) k) (\\p -> (Data.Functor.fmap (\\u -> (f1 u) p)) ((Data.Traversable.mapM (\\t0 -> t0)) gs)))))))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) b) (\\gs -> ((Control.Monad.>>=) c) (\\k -> Control.Applicative.pure (((Control.Monad.>>=) k) (\\p -> (Data.Functor.fmap (\\u -> (f1 u) p)) (sequence gs)))))))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) c) (\\g -> ((Control.Monad.>>=) b) (\\ks -> Control.Applicative.pure (((Control.Monad.>>=) g) (\\p -> (Data.Functor.fmap (\\u -> (f1 u) p)) (sequence ks)))))))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) c) (\\g -> (Data.Functor.fmap (\\ks -> ((Control.Monad.>>=) ((Data.Traversable.mapM (\\t -> t)) ks)) (\\r -> (Data.Functor.fmap (f1 r)) g))) b)))"]
                                     []
  , (,,,,,) "liftSBlubS" False False "Monad m => ([a] -> b -> c) -> m [Data.Maybe.Maybe a] -> m (Data.Maybe.Maybe b) -> m (Data.Maybe.Maybe c)"
                                     ["\\f1 -> Control.Applicative.liftA2 (\\i -> (\\j -> ((Control.Monad.>>=) j) (\\n -> (Data.Functor.fmap (\\r -> (f1 r) n)) (Data.Traversable.sequenceA i))))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Applicative.liftA2 (\\i -> (\\j -> ((Control.Monad.>>=) i) (\\n -> (Data.Functor.fmap (\\r -> (f1 r) n)) (Data.Traversable.sequenceA j))))) c) b))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) b) (\\gs -> (Data.Functor.fmap (\\m11 -> ((Control.Monad.>>=) (Data.Traversable.sequenceA gs)) (\\p -> (Data.Functor.fmap (f1 p)) m11))) c)))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) c) (\\m7 -> (Data.Functor.fmap (\\ks -> ((Control.Monad.>>=) (Data.Traversable.sequenceA ks)) (\\p -> (Data.Functor.fmap (f1 p)) m7))) b)))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) b) (\\gs -> (Data.Functor.fmap (\\m11 -> ((Control.Monad.>>=) (Prelude.sequence gs)) (\\p -> (Data.Functor.fmap (f1 p)) m11))) c)))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) c) (\\m7 -> (Data.Functor.fmap (\\ks -> ((Control.Monad.>>=) m7) (\\p -> (Data.Functor.fmap (\\t -> (f1 t) p)) ((Data.Traversable.mapM (\\z -> z)) ks)))) b)))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) c) (\\m7 -> ((Control.Monad.>>=) b) (\\ks -> Control.Applicative.pure (((Control.Monad.>>=) m7) (\\p -> (Data.Functor.fmap (\\u -> (f1 u) p)) ((Data.Traversable.mapM (\\t0 -> t0)) ks)))))))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) b) (\\gs -> ((Control.Monad.>>=) c) (\\m11 -> Control.Applicative.pure (((Control.Monad.>>=) m11) (\\p -> (Data.Functor.fmap (\\u -> (f1 u) p)) ((Data.Traversable.mapM (\\t0 -> t0)) gs)))))))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) b) (\\gs -> ((Control.Monad.>>=) c) (\\m11 -> Control.Applicative.pure (((Control.Monad.>>=) m11) (\\p -> (Data.Functor.fmap (\\u -> (f1 u) p)) (sequence gs)))))))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) c) (\\m7 -> ((Control.Monad.>>=) b) (\\ks -> Control.Applicative.pure (((Control.Monad.>>=) m7) (\\p -> (Data.Functor.fmap (\\u -> (f1 u) p)) (sequence ks)))))))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) c) (\\m7 -> (Data.Functor.fmap (\\ks -> ((Control.Monad.>>=) ((Data.Traversable.mapM (\\t -> t)) ks)) (\\r -> (Data.Functor.fmap (f1 r)) m7))) b)))"
                                     ,"\\f1 -> (\\b -> (\\c -> ((Control.Monad.>>=) c) (Data.Traversable.mapM (\\ks -> (Data.Functor.fmap (\\p -> (f1 (fold ((Data.Traversable.mapM (\\w -> w)) p))) ks)) b))))"]
                                     []
  , (,,,,,) "joinBlub"   False False "Monad m => [Decl] -> (Decl -> m [FunctionBinding]) -> m [FunctionBinding]"
                                     ["\\as -> (\\f2 -> ((Control.Monad.>>=) ((Data.Traversable.traverse f2) as)) (\\i -> Control.Applicative.pure (Control.Monad.join i)))"
                                     ,"\\as -> (\\f2 -> ((Control.Monad.>>=) ((Data.Traversable.traverse f2) as)) (((.) Control.Applicative.pure) Control.Monad.join))"
                                     ,"\\as -> (\\f2 -> ((Control.Monad.>>=) ((Data.Traversable.mapM f2) as)) (\\i -> Control.Applicative.pure (((Control.Monad.>>=) i) (\\p -> p))))"
                                     ,"\\as -> (\\f2 -> (Data.Functor.fmap (\\g -> ((Control.Monad.>>=) g) (\\k -> k))) ((Data.Traversable.mapM f2) as))"
                                     ,"\\as -> (\\f2 -> ((Control.Monad.>>=) ((Data.Traversable.mapM f2) as)) (\\l -> Control.Applicative.pure (((Control.Monad.>>=) l) (\\p -> p))))"
                                     ,"\\as -> (\\f2 -> ((Control.Monad.>>=) ((Data.Traversable.mapM f2) as)) (\\l -> Control.Applicative.pure (concat l)))"
                                     ,"\\as -> (\\f2 -> ((Control.Monad.>>=) ((Data.Traversable.mapM f2) as)) (\\i -> (Data.Traversable.mapM (\\p -> p)) (((Control.Monad.>>=) i) (Data.Functor.fmap Control.Applicative.pure))))"]
                                     []
  , (,,,,,) "liftA2"     False False "Applicative f => (a -> b -> c) -> f a -> f b -> f c"
                                     ["\\f1 -> (\\f2 -> (Control.Applicative.<*>) (((Control.Applicative.<*>) (Control.Applicative.pure f1)) f2))"
                                     ,"\\f1 -> (\\f2 -> (\\d -> ((Control.Applicative.<*>) ((Data.Functor.fmap (\\j -> (\\k -> (f1 k) j))) d)) f2))"
                                     ,"\\f1 -> (\\f2 -> (Control.Applicative.<*>) ((Data.Functor.fmap f1) f2))"
                                     ,"\\f1 -> ((.) (Control.Applicative.<*>)) (Data.Functor.fmap f1)"
                                     ]
                                     ["liftA2", "liftA3"]
  , (,,,,,) "runEitherT" False False "Monad m => [D] -> (D -> Control.Monad.Trans.Either.EitherT e m [FB]) -> ([FB] -> [FB]) -> m [Data.Either.Either e [FB]]"
                                     ["\\as -> (\\f2 -> (\\f3 -> (Data.Traversable.traverse (\\h -> Control.Monad.Trans.Either.runEitherT (((Control.Monad.>>=) (f2 h)) (\\n -> Control.Applicative.pure (f3 n))))) as))"
                                     ,"\\as -> (\\f2 -> (\\f3 -> (Data.Traversable.traverse (((.) Control.Monad.Trans.Either.runEitherT) (((.) (Data.Functor.fmap f3)) f2))) as))"
                                     ,"\\as -> (\\f2 -> (\\f3 -> (Data.Traversable.traverse (\\h -> Control.Monad.Trans.Either.runEitherT ((Data.Functor.fmap f3) (f2 h)))) as))"
                                     ,"\\as -> (\\f2 -> (\\f3 -> (Data.Traversable.mapM (\\h -> Control.Monad.Trans.Either.runEitherT ((Data.Functor.fmap f3) (f2 h)))) as))"]
                                     []
  , (,,,,,) "constr"     False False "(Monad m, Ord e) => ((e -> Data.Either.Either e TC) -> A -> Control.Monad.Trans.Either.EitherT e m C) -> Data.Either.Either e TC -> Data.Map.Map e (Data.Either.Either e TC) -> [A] -> Control.Monad.Trans.Either.EitherT e m [C]"
                                     ["\\f1 -> (\\e2 -> (\\m3 -> Data.Traversable.traverse (f1 (\\l -> (Data.Maybe.fromMaybe e2) ((Data.Map.lookup l) m3)))))"
                                     ,"\\f1 -> (\\e2 -> (\\m3 -> Data.Traversable.mapM (f1 (\\l -> (Data.Maybe.fromMaybe e2) ((Data.Map.lookup l) m3)))))"
                                     ,"\\f1 -> (\\e2 -> (\\m3 -> Data.Traversable.mapM (f1 (\\l -> ((Data.Maybe.maybe e2) (\\q -> q)) ((Data.Map.lookup l) m3)))))"]
                                     []
  , (,,,,,) "fmapmap"    False False "Monad m => T -> [N] -> (CT -> N -> FB) -> (SC -> T -> m CT) -> SC -> m [FB]"
                                     ["\\t1 -> (\\bs -> (\\f3 -> (\\f4 -> (\\s5 -> ((Control.Monad.>>=) ((f4 s5) t1)) (\\c11 -> (Data.Traversable.traverse (((.) Control.Applicative.pure) (f3 c11))) bs)))))"
                                     ,"\\t1 -> (\\bs -> (\\f3 -> (\\f4 -> (\\s5 -> ((Control.Monad.>>=) ((f4 s5) t1)) (\\c11 -> (Data.Traversable.traverse (\\p -> Control.Applicative.pure ((f3 c11) p))) bs)))))"
                                     ,"\\t1 -> (\\bs -> (\\f3 -> (\\f4 -> (\\s5 -> ((Control.Monad.>>=) ((f4 s5) t1)) (\\c11 -> (Data.Traversable.traverse (((.) Control.Applicative.pure) (f3 c11))) bs)))))"
                                     ,"\\t1 -> (\\bs -> (\\f3 -> (\\f4 -> (\\s5 -> ((>>=) ((f4 s5) t1)) (\\c11 -> (Data.Traversable.traverse (\\p -> pure ((f3 c11) p))) bs)))))"
                                     ,"\\t1 -> (\\bs -> (\\f3 -> (\\f4 -> (\\s5 -> ((Control.Monad.>>=) ((f4 s5) t1)) (\\c11 -> (Data.Traversable.mapM (\\p -> Control.Applicative.pure ((f3 c11) p))) bs)))))"
                                     ,"\\t1 -> (\\bs -> (\\f3 -> (\\f4 -> (\\s5 -> (Data.Traversable.mapM (\\j -> (Data.Functor.fmap (\\n -> (f3 n) j)) ((f4 s5) t1))) bs))))"
                                     ]
                                     []
  , (,,,,,) "fmapmap2"   False False "Monad m => T -> SC -> (T -> m [FB] -> m [FB]) -> [N] -> (SC -> T -> m CT) -> (CT -> N -> FB) -> m [FB]"
                                     ["\\t1 -> (\\s2 -> (\\f3 -> (\\ds -> (\\f5 -> (\\f6 -> (f3 t1) ((Data.Traversable.traverse (\\n12 -> ((Control.Monad.>>=) ((f5 s2) t1)) (\\s -> Control.Applicative.pure ((f6 s) n12)))) ds))))))"
                                     ,"\\t1 -> (\\s2 -> (\\f3 -> (\\ds -> (\\f5 -> (\\f6 -> (f3 t1) ((Data.Traversable.traverse (\\n12 -> (Data.Functor.fmap (\\c16 -> (f6 c16) n12)) ((f5 s2) t1))) ds))))))"
                                     ,"\\t1 -> (\\s2 -> (\\f3 -> (\\ds -> (\\f5 -> (\\f6 -> (f3 t1) ((Data.Traversable.mapM (\\n12 -> (Data.Functor.fmap (\\s -> (f6 s) n12)) ((f5 s2) t1))) ds))))))"
                                     ,"\\t1 -> (\\s2 -> (\\f3 -> (\\ds -> (\\f5 -> (\\f6 -> (f3 t1) ((Data.Traversable.mapM (\\n12 -> (Data.Functor.fmap (\\c16 -> (f6 c16) n12)) ((f5 s2) t1))) ds))))))"
                                     ,"\\t1 -> (\\s2 -> (\\f3 -> (\\ds -> (\\f5 -> (\\f6 -> (f3 t1) ((Data.Traversable.mapM (\\n12 -> ((Control.Monad.>>=) ((f5 s2) t1)) (\\s -> Control.Applicative.pure ((f6 s) n12)))) ds))))))"]
                                     []
  , (,,,,,) "contRet"    False False "a -> Control.Monad.Trans.Cont.Cont r a"
                                     ["\\a -> Control.Monad.Trans.Cont.Cont (\\f4 -> f4 a)"]
                                     []
  , (,,,,,) "contBind"   False False "Control.Monad.Trans.Cont.Cont r a -> (a -> Control.Monad.Trans.Cont.Cont r b) -> Control.Monad.Trans.Cont.Cont r b"
                                     ["\\c1 -> (\\f2 -> let (Control.Monad.Trans.Cont.Cont f4) = c1 in Control.Monad.Trans.Cont.Cont (\\f6 -> f4 (\\i -> let (Control.Monad.Trans.Cont.Cont f13) = f2 i in f13 f6)))"]
                                     []
  , (,,,,,) "ap"         False False "Monad m => m (a->b) -> m a -> m b"
                                     ["(Control.Applicative.<*>)"]
                                     []
  ]

{-
  , (,) "liftBlub"
    (ExpLambda 1
      (ExpLambda 2
        (ExpLambda 3
          (ExpApply
            (ExpApply (ExpLit "(>>=)") (ExpVar 1))
            (ExpLambda 7
              (ExpApply
                (ExpApply (ExpLit "(>>=)") (ExpVar 2))
                (ExpLambda 11
                  (ExpApply
                    (ExpApply (ExpVar 3) (ExpVar 7))
                    (ExpVar 11)))))))))
-}

exampleInput :: [(String, Bool, Bool, String)]
exampleInput =
  [ (,,,) "State"      False False "(s -> (a, s)) -> State s a"
  , (,,,) "showmap"    False False "(Show b) => (a -> b) -> [a] -> [String]"
  , (,,,) "ffbind"     False False "(a -> t -> b) -> (t -> a) -> (t -> b)"
  , (,,,) "join"       False False "(Monad m) => m (m a) -> m a"
  , (,,,) "fjoin"      False False "(t -> (t -> a)) -> t -> a"
  , (,,,) "zipThingy"  False False "[a] -> b -> [(a, b)]"
  , (,,,) "stateRun"   True  False "State a b -> a -> b"
  , (,,,) "fst"        True  False "(a, b) -> a"
  , (,,,) "ffst"       True  False "(a -> (b, c)) -> a -> b"
  , (,,,) "snd"        True  False "(a, b) -> b"
  , (,,,) "quad"       False False "a -> ((a, a), (a, a))"
  , (,,,) "fswap"      False False "(a -> (b, c)) -> a -> (c, b)"
  , (,,,) "liftBlub"   False False "Monad m => m a -> m b -> (a -> b -> m c) -> m c"
  , (,,,) "stateBind"  False False "State s a -> (a -> State s b) -> State s b"
  , (,,,) "dbMaybe"    False False "Maybe a -> Maybe (a, a)"
  , (,,,) "tupleShow"  False False "Show a, Show b => (a, b) -> String"
  , (,,,) "FloatToInt" False False "Float -> Int"
  , (,,,) "FloatToIntL" False False "[Float] -> [Int]"
  , (,,,) "longApp"    False False "a -> b -> c -> (a -> b -> d) -> (a -> c -> e) -> (b -> c -> f) -> (d -> e -> f -> g) -> g"
  , (,,,) "liftSBlub"  False False "Monad m, Monad n => ([a] -> b -> c) -> m ([n a]) -> m (n b) -> m (n c)"
  , (,,,) "liftSBlubS" False False "Monad m => (List a -> b -> c) -> m ([Maybe a]) -> m (Maybe b) -> m (Maybe c)"
  , (,,,) "joinBlub"   False False "Monad m => [Decl] -> (Decl -> m [FunctionBinding] -> m [FunctionBinding]"
  , (,,,) "liftA2"     False False "Applicative f => (a -> b -> c) -> f a -> f b -> f c"
  ]

filterBindings :: (QualifiedName -> Bool)
               -> [FunctionBinding]
               -> [FunctionBinding]
filterBindings p = filter $ \(_, qn, _, _, _) -> p qn

filterBindingsSimple :: [String]
                     -> [FunctionBinding]
                     -> [FunctionBinding]
filterBindingsSimple es = filterBindings $ \n -> case n of
  QualifiedName _ name -> name `notElem` es
  _                    -> True

checkInput :: ( m ~ MultiRWST r w s m0
              , Monad m0
              , Functor m0
              , ContainsType TypeDeclMap r
              )
           => ExferenceHeuristicsConfig
           -> EnvDictionary
           -> String
           -> Bool
           -> Bool
           -> [String]
           -> m ExferenceInput
checkInput heuristics (bindings, deconss, sEnv) typeStr allowUnused patternM hidden = do
  tDeclMap <- mAsk
  ty <- unsafeReadType (sClassEnv_tclasses sEnv) exampleDataTypes tDeclMap typeStr
  let filteredBindings = filterBindingsSimple ("fix":"forever":"iterateM_":hidden) bindings
  return $ ExferenceInput
    ty
    filteredBindings
    deconss
    sEnv
    allowUnused
    False
    8192
    patternM
    20000
    (Just 8192)
    heuristics

exampleDataTypes :: [QualifiedName]
exampleDataTypes
  = parseQualifiedName <$> [ "Data.String.String"
                           , "Prelude.Float"
                           , "Data.Int.Int"
                           , "Data.Bool.Bool"
                           ]

checkExpectedResults :: forall m m0 r w s
                      . ( m ~ MultiRWST r w s m0
                        , Monad m0
                        , Functor m0
                        , ContainsType TypeDeclMap r
                        )
                     => ExferenceHeuristicsConfig
                     -> EnvDictionary
                     -> m [ ( String -- ^ name
                            , [String] -- ^ expected
                            , Maybe ( (Expression, ExferenceStats)
                                      -- ^ first
                                    , Maybe (Int, ExferenceStats)
                                    ) -- ^ index and stats of expected
                            )]
checkExpectedResults heuristics env = mapMultiRWST (return . runIdentity)
                                      -- for lazyness, we drop the IO
                                    $ sequence
  [ [ (name, expected, r)
    | input <- checkInput heuristics env typeStr allowUnused patternM hidden
    , let getExp :: (Expression, [HsConstraint], ExferenceStats)
                 -> MaybeT (MultiRWST r w s Identity) ExferenceStats
          getExp (e, _, s) =
            [ s
            | showExpression (simplifyExpression e) `elem` expected
            ]
    , let xs = findExpressions input
    , r <- runMaybeT
        [ ((simplifyExpression e, stats), rs)
        | let ((e, _, stats):_) = xs
        , rs <- lift $ runMaybeT $ asum $ zipWith (fmap . (,)) [0..] $ map getExp xs
        ]
    ]
  | (name, allowUnused, patternM, typeStr, expected, hidden) <- checkData
  ]


{-
checkBestResults :: ExferenceHeuristicsConfig
                 -> EnvDictionary
                 -> [ ( String
                      , [String]
                      , Maybe ( (Expression, ExferenceStats)
                                  -- ^ first result
                              , (Int, Expression, ExferenceStats)
                                  -- ^ best result
                              , Maybe (Int, ExferenceStats)
                                  -- ^ expected
                              )
                      )]
checkBestResults heuristics env = do
  (name, allowUnused, typeStr, expected) <- checkData
  let input = checkInput heuristics env typeStr allowUnused
  let getBest :: [(Expression, ExferenceStats)]
              -> (Int, Expression, ExferenceStats)
      getBest = maximumBy (comparing g) . zipWith (\a (b,c) -> (a,b,c)) [0..]
        where
          g (_,_,ExferenceStats _ f) = f
  let getExp :: Int
             -> [(Expression, ExferenceStats)]
             -> Maybe (Int, ExferenceStats)
      getExp _ [] = Nothing
      getExp n ((e,s):r) | show e `elem` expected = Just (n,s)
                         | otherwise              = getExp (n+1) r
  return $
    ( name
    , expected
    , case findExpressions input of
        [] -> Nothing
        xs@(x:_) -> Just (x, getBest xs, getExp 0 xs)
    )
-}

{-
checkBestResultsPar :: ExferenceHeuristicsConfig
                    -> EnvDictionary
                    -> [ IO ( String
                            , [String]
                            , Maybe ( (Expression, ExferenceStats)
                                        -- ^ first result
                                    , (Int, Expression, ExferenceStats)
                                        -- ^ best result
                                    , Maybe (Int, ExferenceStats)
                                        -- ^ expected
                                    )
                            )]
-}

{-
checkResults :: ExferenceHeuristicsConfig
             -> EnvDictionary
             -> [IO ( String -- name
                    , [String] -- expected
                    , Maybe Expression -- first
                    , Maybe Expression -- best
                    , Maybe (Int, ExferenceStats) -- expected was nth solution
                                                  -- and had these stats
                    , [(Expression, ExferenceStats)]
                    )]
checkResults heuristics (bindings, sEnv) = do
  (name, allowUnused, typeStr, expected) <- checkData
  let input = ExferenceInput
                (readConstrainedType sEnv typeStr)
                (filter (\(x,_,_) -> x/="join" && x/="liftA2") bindings)
                sEnv
                allowUnused
                131072
                (Just 131072)
                heuristics
  let r = findExpressionsPar input
  let finder :: Int
             -> [(Expression, ExferenceStats)]
             -> Maybe (Int, ExferenceStats)
      finder n [] = Nothing
      finder n ((e, s):r) | show e `elem` expected = Just (n, s)
                          | otherwise = finder (n+1) r
      bestFound = findSortNExpressions 100 input
  return $ (,,,,,)
         <$> return name
         <*> return expected
         <*> fmap fst <$> findOneExpressionPar input
         -- <*> return (fst <$> findOneExpression input)
         <*> return (fst <$> listToMaybe bestFound)
         <*> (finder 0 <$> r)
         <*> r
-}

exampleOutput :: ( m ~ MultiRWST r w s m0
                 , Monad m0
                 , Functor m0
                 )
              => ExferenceHeuristicsConfig
              -> EnvDictionary
              -> m [[(Expression, [HsConstraint], ExferenceStats)]]
exampleOutput heuristics (bindings, deconss, sEnv) =
  exampleInput `forM` \(_, allowUnused, patternM, s) -> do
    ty <- unsafeReadType (sClassEnv_tclasses sEnv) exampleDataTypes (M.empty) s
    let filteredBindings = filterBindingsSimple ["join", "liftA2"] bindings
    return $ takeFindSortNExpressions 10 10 $ ExferenceInput
                ty
                filteredBindings
                deconss
                sEnv
                allowUnused
                False
                8192
                patternM
                32768
                (Just 32768)
                heuristics

exampleInOut :: ( m ~ MultiRWST r w s m0
                , Monad m0
                , Functor m0
                )
             => ExferenceHeuristicsConfig
             -> EnvDictionary
             -> m [( (String, Bool, Bool, String)
                   , [(Expression, [HsConstraint], ExferenceStats)]
                   )]
exampleInOut h env =
  zip exampleInput <$> exampleOutput h env

printAndStuff :: ExferenceHeuristicsConfig
              -> EnvDictionary
              -> MultiRWST r w s IO ()
printAndStuff h env = exampleInOut h env >>= mapM_ f
  where
    f ((name, _, _, _), []) = lift $ putStrLn $ "no results for "++name++"!"
    f ((name, _, _, _), results) = mapM_ g results
      where
        g (expr, _, ExferenceStats n d m) = do
          {-
          if doPf then do
            pf <- pointfree $ str
            putStrLn $ name ++ " = " ++ pf
                       ++ "    FROM    " ++ name ++ " = " ++ str
                       ++ " (depth " ++ show d ++ ", " ++ show n ++ " steps)"
           else
          -}
          lift $ putStrLn $ name ++ " = " ++ showExpression expr
                            ++ " (depth "
                            ++ show d
                            ++ ", "
                            ++ show n
                            ++ " steps, "
                            ++ show m
                            ++ " max pqueue size)"

printStatistics :: ExferenceHeuristicsConfig
                -> EnvDictionary
                -> MultiRWST r w s IO ()
printStatistics h env = exampleInOut h env >>= mapM_ f
  where
    f ((name, _, _, _), [])      = lift $ putStrLn $ printf "%10s: ---" name
    f ((name, _, _, _), results) =
      let (hd, avg, minV, maxV, n) = getStats results
      in lift $ putStrLn
              $ printf "%12s: head=%6d avg=%6d min=%6d max=%6d %s" name hd avg minV maxV
                                     (if n==6 then "" else " n = " ++ show n)
    getStats results =
      let steps = map (\(_, _, stats) -> exference_steps stats) results
      in ( head steps
         , sum steps `div` length steps
         , minimum steps
         , maximum steps
         , length steps
         )

printCheckExpectedResults :: forall r w s
                           . ( ContainsType TypeDeclMap r
                             )
                          => ExferenceHeuristicsConfig
                          -> EnvDictionary
                          -> MultiRWST r w s IO ()
printCheckExpectedResults h env = do
    xs <- checkExpectedResults h env
    case () of { () -> do
    stats <- mapM helper xs
    lift $ putStrLn $ "total:     " ++ show (length stats)
    lift $ putStrLn $ "solutions: " ++ (show
                                       $ length
                                       $ catMaybes
                                       $ fst <$> stats)
    lift $ putStrLn $ "success:   " ++ ( show
                                       $ length
                                       $ filter id
                                       $ catMaybes
                                       $ uncurry (liftM2 (==)) <$> stats)
    lift $ putStrLn $ "rating any solutions: "
             ++ ( show
                $ foldr g (0, 0.0)
                $ fromMaybe (ExferenceStats 1000000 1000000 0) . fst <$> stats)
    lift $ putStrLn $ "rating good solutions: "
             ++ ( show
                $ foldr g (0, 0.0)
                $ fromMaybe (ExferenceStats 1000000 1000000 0) . snd <$> stats)
  where
    helper :: ( String -- ^ name
              , [String] -- ^ expected
              , Maybe ( (Expression, ExferenceStats) -- ^ first
                      , Maybe (Int, ExferenceStats)
                      ) -- ^ index and stats of expected
              )
           -> MultiRWST r w s IO (Maybe ExferenceStats, Maybe ExferenceStats)
    helper (name, _, Nothing) = do
      lift $ putStrLn $ printf "%-12s: no solutions found at all!" name
      return (Nothing, Nothing)
    helper (name, e, Just ((first,stats), Nothing)) = do
      lift $ putStrLn $ printf "%-12s: expected solution not found!" name
      let firstStr = showExpression first
      lift $ putStrLn $ "  first solution:       " ++ firstStr
      lift $ putStrLn $ "  first solution stats: " ++ show stats
      lift $ putStrLn $ "  expected solutions:   " ++ intercalate
                     "\n                      or " e
      lift $ putStrLn $ "  " ++ show firstStr
      return (Just stats, Nothing)
    helper (name, _, Just (_, Just (0, stats))) = do
      lift $ putStrLn $ printf "%-12s: %s" name (show stats)
      return (Just stats, Just stats)
    helper (name, e, Just ((first, fstats), Just (n, stats))) = do
      lift $ putStrLn $ printf "%-12s: expected solution not first, but %d!" name n
      lift $ putStrLn $ "  first solution:     " ++ showExpression first
      lift $ putStrLn $ "  expected solutions:   " ++ intercalate
                      "\n                     or " e
      lift $ putStrLn $ "  first solution stats:    " ++ show fstats
      lift $ putStrLn $ "  expected solution stats: " ++ show stats
      lift $ putStrLn $ "  " ++ show (showExpression first)
      return (Just fstats, Just stats)
    g :: ExferenceStats -> (Int,Float) -> (Int,Float)
    g (ExferenceStats a b _) (d,e) = (a+d,b+e)
  }

printMaxUsage :: ExferenceHeuristicsConfig
              -> EnvDictionary
              -> MultiRWST r w s IO ()
printMaxUsage h (bindings, deconss, sEnv) = sequence_ $ do
  (name, allowUnused, patternM, typeStr, _expected, hidden) <- checkData
  return $ do
    ty <- unsafeReadType (sClassEnv_tclasses sEnv) exampleDataTypes (M.empty) typeStr
    let filteredBindings = filterBindingsSimple hidden bindings
    let input = ExferenceInput
                  ty
                  filteredBindings
                  deconss
                  sEnv
                  allowUnused
                  False
                  8192
                  patternM
                  16384
                  (Just 16384)
                  h
    let stats = chunkBindingUsages $ last $ findExpressionsWithStats input
        highest = take 5 $ sortBy (flip $ comparing snd) $ M.toList stats
    lift $ putStrLn $ printf "%-12s: %s" name (show highest)

#if BUILD_SEARCH_TREE
printSearchTree :: ( ContainsType QNameIndex s )
                => ExferenceHeuristicsConfig
                -> EnvDictionary
                -> MultiRWST r w s IO ()
printSearchTree h (bindings, deconss, sEnv) = sequence_ $ do
  (name, allowUnused, patternM, typeStr, _expected, hidden) <- checkData
  return $ do
    ty <- unsafeReadType (sClassEnv_tclasses sEnv) exampleDataTypes (M.empty) typeStr
    filteredBindings <- filterBindingsSimple hidden bindings
    let input = ExferenceInput
                  ty
                  filteredBindings
                  deconss
                  sEnv
                  allowUnused
                  False
                  8192
                  patternM
                  256
                  (Just 256)
                  h
    let tree = chunkSearchTree $ last $ findExpressionsWithStats input
    let showf (total,processed,expression)
          = printf "%d (+%d): %s" processed
                                  (total-processed)
                                  (showExpressionPure qNameIndex expression)
    lift $ putStrLn
         $ name
    lift $ putStrLn
         $ drawTree
         $ fmap showf
         $ filterSearchTreeProcessedN 2
         $ tree
#endif

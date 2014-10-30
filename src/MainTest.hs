module MainTest
  ( printAndStuff
  , printCheckExpectedResults
  , printStatistics
  , printMaxUsage
  , printSearchTree
  )
where



import Language.Haskell.ExferenceCore ( ExferenceHeuristicsConfig(..)
                                      , findExpressionsWithStats )
import Language.Haskell.Exference
import Language.Haskell.Exference.ExpressionToHaskellSrc
import Language.Haskell.Exference.BindingsFromHaskellSrc
import Language.Haskell.Exference.ContextFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.FunctionBinding

import Language.Haskell.Exference.ConstrainedType
import Language.Haskell.Exference.TypeClasses
import Language.Haskell.Exference.Expression
import Language.Haskell.Exference.ExferenceStats
import Language.Haskell.Exference.SearchTree

import Control.DeepSeq

import System.Process

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( second, (***) )
import Control.Monad ( when, forM_, guard, forM, mplus, mzero )
import Data.List ( sortBy, find, intercalate, maximumBy )
import Data.Ord ( comparing )
import Text.Printf
import Data.Maybe ( listToMaybe, fromMaybe, maybeToList, catMaybes )
import Data.Either ( lefts, rights )
import Control.Monad.Writer.Strict
import qualified Data.Map as M
import Data.Tree ( drawTree )
import qualified ListT

import Language.Haskell.Exts.Syntax ( Module(..), Decl(..), ModuleName(..) )
import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , parseModule
                                    , ParseResult (..)
                                    , ParseMode (..) )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )

import Data.PPrint

import Debug.Hood.Observe



checkData :: [(String, Bool, String, [String])]
checkData =
  [ (,,,) "showmap"    False "(Show b) => (a -> b) -> List a -> List String"
                             ["\\b -> fmap (\\g -> show (b g))"
                             ,"\\b -> (\\c -> ((>>=) c) (\\g -> pure (show (b g))))"]
  , (,,,) "ffbind"     False "(a -> t -> b) -> (t -> a) -> (t -> b)"
                             ["\\b -> (\\c -> (\\d -> (b (c d)) d))"]
  , (,,,) "join"       False "(Monad m) => m (m a) -> m a"
                             ["\\b -> ((>>=) b) (\\f -> f)"]
  , (,,,) "fjoin"      False "(t -> (t -> a)) -> t -> a"
                             ["\\b -> (\\c -> (b c) c)"]
  , (,,,) "zipThingy"  False "List a -> b -> List (Tuple2 a b)"
                             ["\\b -> (\\c -> ((fmap (\\g -> ((,) g) c)) b)"
                             ,"\\b -> (\\c -> (zip b) (pure c))"]
  , (,,,) "stateRun"   True  "State a b -> a -> b"
                             ["\\b -> (\\c -> let (State e) = b in let ((,) h i) = e c in h)"]
  , (,,,) "fst"        True  "Tuple2 a b -> a"
                             ["\\b -> let ((,) d e) = b in d"]
  --, (,,,) "ffst"       True  "(a -> Tuple b c) -> a -> b"
  , (,,,) "snd"        True  "Tuple2 a b -> b"
                             ["\\b -> let ((,) d e) = b in e"]
  , (,,,) "quad"       False "a -> Tuple2 (Tuple2 a a) (Tuple2 a a)"
                             ["\\b -> ((,) (((,) b) b)) (((,) b) b)"]
  -- , (,,,) "fswap"      False "(a -> Tuple b c) -> a -> Tuple c b"
  , (,,,) "liftBlub"   False "Monad m => m a -> m b -> (a -> b -> m c) -> m c"
                             ["\\b -> (\\c -> (\\d -> ((>>=) b) (\\h -> ((>>=) c) (d h))))"
                             ,"\\b -> (\\c -> (\\d -> ((>>=) c) (\\h -> ((>>=) b) (\\l -> (d l) h))))"]
  , (,,,) "stateBind"  False "State s a -> (a -> State s b) -> State s b"
                             ["\\b -> (\\c -> let (State e) = b in State (\\g -> let ((,) k l) = e g in let (State o) = c k in o l))"]
  , (,,,) "dbMaybe"    False "Maybe a -> Maybe (Tuple2 a a)"
                             ["fmap (\\f -> ((,) f) f)"
                             ,"\\b -> ((liftM2 (\\g -> (\\h -> ((,) h) g))) b) b"
                             ,"\\b -> ((>>=) b) (\\f -> pure (((,) f) f))"]
  , (,,,) "tupleShow"  False "Show a, Show b => Tuple2 a b -> String"
                             ["show"
                             ,"\\b -> let ((,) d e) = b in show (((,) d) e)"]
  , (,,,) "FloatToInt" False "Float -> Int"
                             ["truncate"]
  , (,,,) "FloatToIntL" False "List Float -> List Int"
                             ["fmap round"
                             ,"fmap floor"
                             ,"fmap ceiling"
                             ,"fmap truncate"
                             ,"\\b -> ((>>=) b) (\\f -> pure (truncate f))" -- this is kind of ugly
                             ]
  , (,,,) "longApp"    False "a -> b -> c -> (a -> b -> d) -> (a -> c -> e) -> (b -> c -> f) -> (d -> e -> f -> g) -> g"
                             ["\\b -> (\\c -> (\\d -> (\\e -> (\\f -> (\\g -> (\\h -> ((h ((e b) c)) ((f b) d)) ((g c) d)))))))"]
  , (,,,) "liftSBlub"  False "Monad m, Monad n => (List a -> b -> c) -> m (List (n a)) -> m (n b) -> m (n c)"
                             ["\\b -> (\\c -> (\\d -> ((>>=) d) (\\h -> (fmap (\\l -> ((>>=) h) (\\p -> (fmap (\\t -> (b t) p)) ((mapM (\\z -> z)) l)))) c)))"
                             ,"\\b -> (\\c -> (\\d -> ((>>=) d) (\\h -> ((>>=) c) (\\l -> pure (((>>=) h) (\\q -> (fmap (\\u -> (b u) q)) ((mapM (\\t0 -> t0)) l)))))))"
                             ,"\\b -> (\\c -> (\\d -> ((>>=) c) (\\h -> ((>>=) d) (\\l -> pure (((>>=) l) (\\q -> (fmap (\\u -> (b u) q)) ((mapM (\\t0 -> t0)) h)))))))"
                             ,"\\b -> (\\c -> (\\d -> ((>>=) c) (\\h -> ((>>=) d) (\\l -> pure (((>>=) l) (\\q -> (fmap (\\u -> (b u) q)) (sequence h)))))))"
                             ,"\\b -> (\\c -> (\\d -> ((>>=) d) (\\h -> ((>>=) c) (\\l -> pure (((>>=) h) (\\q -> (fmap (\\u -> (b u) q)) (sequence l)))))))"]
  , (,,,) "liftSBlubS" False "Monad m => (List a -> b -> c) -> m (List (Maybe a)) -> m (Maybe b) -> m (Maybe c)"
                             ["\\b -> (\\c -> (\\d -> ((>>=) d) (\\h -> (fmap (\\l -> ((>>=) h) (\\p -> (fmap (\\t -> (b t) p)) ((mapM (\\z -> z)) l)))) c)))"
                             ,"\\b -> (\\c -> (\\d -> ((>>=) d) (\\h -> ((>>=) c) (\\l -> pure (((>>=) h) (\\q -> (fmap (\\u -> (b u) q)) ((mapM (\\t0 -> t0)) l)))))))"
                             ,"\\b -> (\\c -> (\\d -> ((>>=) c) (\\h -> ((>>=) d) (\\l -> pure (((>>=) l) (\\q -> (fmap (\\u -> (b u) q)) ((mapM (\\t0 -> t0)) h)))))))"
                             ,"\\b -> (\\c -> (\\d -> ((>>=) c) (\\h -> ((>>=) d) (\\l -> pure (((>>=) l) (\\q -> (fmap (\\u -> (b u) q)) (sequence h)))))))"
                             ,"\\b -> (\\c -> (\\d -> ((>>=) d) (\\h -> ((>>=) c) (\\l -> pure (((>>=) h) (\\q -> (fmap (\\u -> (b u) q)) (sequence l)))))))"]
  , (,,,) "joinBlub"   False "Monad m => List Decl -> (Decl -> m (List FunctionBinding)) -> m (List FunctionBinding)"
                             ["\\b -> (\\c -> ((>>=) ((mapM c) b)) (\\i -> pure (((>>=) i) (\\q -> q))))"
                             ,"\\b -> (\\c -> (fmap (\\g -> ((>>=) g) (\\k -> k))) ((mapM c) b))"
                             ,"\\b -> (\\c -> ((>>=) ((mapM c) b)) (\\l -> pure (((>>=) l) (\\q -> q))))"
                             ,"\\b -> (\\c -> ((>>=) ((mapM c) b)) (\\l -> pure (concat l)))"]
  , (,,,) "liftA2"     False "Applicative f => (a -> b -> c) -> f a -> f b -> f c"
                             ["\\b -> (\\c -> (\\d -> ((<*>) ((fmap (\\j -> (\\k -> (b k) j))) d)) c))"
                             ,"\\b -> (\\c -> (<*>) ((fmap b) c))"]
  , (,,,) "runEitherT" False "Monad m => List D -> (D -> EitherT e m (List FB)) -> (List FB -> List FB) -> m (List (Either e (List FB)))"
                             ["\\b -> (\\c -> (\\d -> (mapM (\\h -> runEitherT ((fmap d) (c h)))) b))"]
  , (,,,) "constr"     False "(Monad m, Ord e) => ((e -> Either e TC) -> A -> EitherT e m C) -> Either e TC -> Map e (Either e TC) -> List A -> EitherT e m (List C)"
                             ["\\b -> (\\c -> (\\d -> mapM (b (\\m -> (fromMaybe c) ((mapLookup m) d)))))"
                             ,"\\b -> (\\c -> (\\d -> mapM (b (\\m -> ((maybe c) (\\r -> r)) ((mapLookup m) d)))))"]
  , (,,,) "fmapmap"    False "Monad m => T -> List N -> (CT -> N -> FB) -> (SC -> T -> m CT) -> SC -> m (List FB)"
                             ["\\b -> (\\c -> (\\d -> (\\e -> (\\f -> ((>>=) ((e f) b)) (\\l -> (mapM (\\p -> pure ((d l) p))) c)))))"]
  , (,,,) "fmapmap2"   False "Monad m => T -> SC -> (T -> m (List FB) -> m (List FB)) -> List N -> (SC -> T -> m CT) -> (CT -> N -> FB) -> m (List FB)"
                             ["\\b -> (\\c -> (\\d -> (\\e -> (\\f -> (\\g -> (d b) ((mapM (\\m -> (fmap (\\s -> (g s) m)) ((f c) b))) e))))))"
                             ,"\\b -> (\\c -> (\\d -> (\\e -> (\\f -> (\\g -> (d b) ((mapM (\\m -> (fmap (\\q -> (g q) m)) ((f c) b))) e))))))"
                             ,"\\b -> (\\c -> (\\d -> (\\e -> (\\f -> (\\g -> (d b) ((mapM (\\m -> ((>>=) ((f c) b)) (\\s -> pure ((g s) m)))) e))))))"]
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

exampleInput :: [(String, Bool, String)]
exampleInput = 
  [ (,,) "State"      False "(s -> Tuple2 a s) -> State s a"
  , (,,) "showmap"    False "(Show b) => (a -> b) -> List a -> List String"
  , (,,) "ffbind"     False "(a -> t -> b) -> (t -> a) -> (t -> b)"
  , (,,) "join"       False "(Monad m) => m (m a) -> m a"
  , (,,) "fjoin"      False "(t -> (t -> a)) -> t -> a"
  , (,,) "zipThingy"  False "List a -> b -> List (Tuple2 a b)"
  , (,,) "stateRun"   True  "State a b -> a -> b"
  , (,,) "fst"        True  "Tuple2 a b -> a"
  , (,,) "ffst"       True  "(a -> Tuple2 b c) -> a -> b"
  , (,,) "snd"        True  "Tuple2 a b -> b"
  , (,,) "quad"       False "a -> Tuple2 (Tuple2 a a) (Tuple2 a a)"
  , (,,) "fswap"      False "(a -> Tuple2 b c) -> a -> Tuple2 c b"
  , (,,) "liftBlub"   False "Monad m => m a -> m b -> (a -> b -> m c) -> m c"
  , (,,) "stateBind"  False "State s a -> (a -> State s b) -> State s b"
  , (,,) "dbMaybe"    False "Maybe a -> Maybe (Tuple2 a a)"
  , (,,) "tupleShow"  False "Show a, Show b => Tuple2 a b -> String"
  , (,,) "FloatToInt" False "Float -> Int"
  , (,,) "FloatToIntL" False "List Float -> List Int"
  , (,,) "longApp"    False "a -> b -> c -> (a -> b -> d) -> (a -> c -> e) -> (b -> c -> f) -> (d -> e -> f -> g) -> g"
  , (,,) "liftSBlub"  False "Monad m, Monad n => (List a -> b -> c) -> m (List (n a)) -> m (n b) -> m (n c)"
  , (,,) "liftSBlubS" False "Monad m => (List a -> b -> c) -> m (List (Maybe a)) -> m (Maybe b) -> m (Maybe c)"
  , (,,) "joinBlub"   False "Monad m => List Decl -> (Decl -> m (List FunctionBinding)) -> m (List FunctionBinding)"
  , (,,) "liftA2"     False "Applicative f => (a -> b -> c) -> f a -> f b -> f c"
  ]

checkInput :: ExferenceHeuristicsConfig
           -> Context
           -> String
           -> Bool
           -> ExferenceInput
checkInput heuristics (bindings, scontext) typeStr allowUnused =
  ExferenceInput
    (readConstrainedType scontext typeStr)
    (filter (\(x,_,_) -> x/="join" && x/="liftA2") bindings)
    scontext
    allowUnused
    262144
    (Just 131072)
    heuristics

checkExpectedResults :: ExferenceHeuristicsConfig
                     -> Context
                     -> [ ( String -- ^ name
                          , [String] -- ^ expected
                          , Maybe ( (Expression, ExferenceStats)
                                    -- ^ first
                                  , Maybe (Int, ExferenceStats)
                                  ) -- ^ index and stats of expected
                          )]
checkExpectedResults heuristics context = do
  (name, allowUnused, typeStr, expected) <- checkData
  let input = checkInput heuristics context typeStr allowUnused
  let getExp :: Int -> [(Expression, ExferenceStats)] -> Maybe (Int, ExferenceStats)
      getExp _ [] = Nothing
      getExp n ((e,s):r) | show e `elem` expected = Just (n,s)
                         | otherwise              = getExp (n+1) r
  return $
    ( name
    , expected
    , case findExpressions input of
      []       -> Nothing
      xs@(x:_) -> Just (x, getExp 0 xs)
    )

checkExpectedResultsPar :: ExferenceHeuristicsConfig
                        -> Context
                        -> [IO ( String -- ^ name
                               , [String] -- ^ expected
                               , Maybe ( (Expression, ExferenceStats)
                                         -- ^ first
                                       , Maybe (Int, ExferenceStats)
                                         -- ^ index and stats of expected
                                       ) 
                               )]
checkExpectedResultsPar heuristics context = do
  (name, allowUnused, typeStr, expected) <- checkData
  let input = checkInput heuristics context typeStr allowUnused
  let getExp :: Int -> [(Expression, ExferenceStats)] -> Maybe (Int, ExferenceStats)
      getExp _ [] = Nothing
      getExp n ((e,s):r) | show e `elem` expected = Just (n,s)
                         | otherwise              = getExp (n+1) r
      helper :: ListT.ListT IO ExferenceOutputElement
             -> IO (Maybe ( (Expression, ExferenceStats)
                          , Maybe (Int, ExferenceStats)))
      helper l = do
        x <- ListT.uncons l
        case x of
          Nothing -> return Nothing
          Just ((e,s), rest)
            | show e `elem` expected
            -> return $ Just ((e,s), Just (0,s))
            | otherwise
            -> do
              exp <- helper2 1 rest
              return $ Just ((e,s), exp)
      helper2 :: Int
              -> ListT.ListT IO ExferenceOutputElement
              -> IO (Maybe (Int, ExferenceStats))
      helper2 n l = do
        x <- ListT.uncons l
        case x of
          Nothing -> return Nothing
          Just ((e,s), rest) | show e `elem` expected
                               -> return $ Just (n,s)
                             | otherwise
                               -> helper2 (n+1) rest
  return $ (,,) name expected <$> findExpressionsPar input helper

checkBestResults :: ExferenceHeuristicsConfig
                 -> Context
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
checkBestResults heuristics context = do
  (name, allowUnused, typeStr, expected) <- checkData
  let input = checkInput heuristics context typeStr allowUnused
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

{-
checkBestResultsPar :: ExferenceHeuristicsConfig
                    -> Context
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
             -> Context
             -> [IO ( String -- name
                    , [String] -- expected
                    , Maybe Expression -- first
                    , Maybe Expression -- best
                    , Maybe (Int, ExferenceStats) -- expected was nth solution
                                                  -- and had these stats
                    , [(Expression, ExferenceStats)]
                    )]
checkResults heuristics (bindings, scontext) = do
  (name, allowUnused, typeStr, expected) <- checkData
  let input = ExferenceInput
                (readConstrainedType scontext typeStr)
                (filter (\(x,_,_) -> x/="join" && x/="liftA2") bindings)
                scontext
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

exampleOutput :: ExferenceHeuristicsConfig
              -> Context
              -> [[(Expression, ExferenceStats)]]
exampleOutput heuristics (bindings, scontext) = map f exampleInput
  where
    f (_, allowUnused, s) = takeFindSortNExpressions 10 10 $ ExferenceInput
                (readConstrainedType scontext s)
                (filter (\(x,_,_) -> x/="join" && x/="liftA2") bindings)
                scontext
                allowUnused
                32768
                (Just 32768)
                heuristics

exampleInOut h context = zip exampleInput (exampleOutput h context)

printAndStuff h context = mapM_ f (exampleInOut h context)
  where
    f ((name, _, _), []) = putStrLn $ "no results for "++name++"!"
    f ((name, _, _), results) = mapM_ g results
      where
        g (expr, ExferenceStats n d) = do
          let str = show expr
              doPf = False
          if doPf then do
            pf <- pointfree $ str
            putStrLn $ name ++ " = " ++ pf
                       ++ "    FROM    " ++ name ++ " = " ++ str
                       ++ " (depth " ++ show d ++ ", " ++ show n ++ " steps)"
           else
            putStrLn $ name ++ " = " ++ str
                       ++ " (depth " ++ show d ++ ", " ++ show n ++ " steps)"

printStatistics h context = mapM_ f (exampleInOut h context)
  where
    f ((name, _, _), [])      = putStrLn $ printf "%10s: ---" name
    f ((name, _, _), results) =
      let (hd, avg, min, max, n) = getStats results
      in putStrLn $ printf "%10s: head=%6d avg=%6d min=%6d max=%6d %s" name hd avg min max
                                     (if n==6 then "" else " n = " ++ show n)
    getStats results =
      let steps = map (exference_steps.snd) results
      in ( head steps
         , sum steps `div` length steps
         , minimum steps
         , maximum steps
         , length steps
         )

printCheckExpectedResults :: Bool -> ExferenceHeuristicsConfig -> Context -> IO ()
printCheckExpectedResults par h context = do
    let xs = if par
          then            checkExpectedResultsPar h context
          else return <$> checkExpectedResults    h context
    stats <- (>>=helper) `mapM` xs
    print $ foldr g (0, 0, 0.0) $ catMaybes $ stats
  where
    helper :: ( String -- ^ name
              , [String] -- ^ expected
              , Maybe ( (Expression, ExferenceStats) -- ^ first
                      , Maybe (Int, ExferenceStats)
                      ) -- ^ index and stats of expected
              )
           -> IO (Maybe ExferenceStats)
    helper (name, _, Nothing) = do
      putStrLn $ printf "%-10s: no solutions found at all!" name
      return Nothing
    helper (name, e, Just ((first,stats), Nothing)) = do
      putStrLn $ printf "%-10s: expected solution not found!" name
      putStrLn $ "  first solution was:   " ++ show first
      putStrLn $ "  first solution stats: " ++ show stats
      putStrLn $ "  expected solutions:   " ++ intercalate ", " e
      putStrLn $ "  " ++ show (show first)
      return Nothing
    helper (name, e, Just (_, Just (0, stats))) = do
      putStrLn $ printf "%-10s: %s" name (show stats)
      return (Just stats)
    helper (name, e, Just ((first, fstats), Just (n, stats))) = do
      putStrLn $ printf "%-10s: expected solution not first, but %d!" name n
      putStrLn $ "  first solution:     " ++ show first
      putStrLn $ "  expected solutions: " ++ intercalate " OR " e
      putStrLn $ "  first solution stats:    " ++ show fstats
      putStrLn $ "  expected solution stats: " ++ show stats
      putStrLn $ "  " ++ show (show first)
      return Nothing
    g :: ExferenceStats -> (Int,Int,Float) -> (Int,Int,Float)
    g (ExferenceStats a b) (c,d,e) = (c+1,a+d,b+e)

printMaxUsage :: ExferenceHeuristicsConfig -> Context -> IO ()
printMaxUsage h (bindings, scontext) = mapM_ f checkData
  where
    f (name, allowUnused, typeStr, _expected) = do
      let input = ExferenceInput
                    (readConstrainedType scontext typeStr)
                    (filter (\(x,_,_) -> x/="join") bindings)
                    scontext
                    allowUnused
                    16384
                    (Just 16384)
                    h
      let (stats, _, _) = last $ findExpressionsWithStats input
          highest = take 5 $ sortBy (flip $ comparing snd) $ M.toList stats
      putStrLn $ printf "%-10s: %s" name (show highest)

printSearchTree :: ExferenceHeuristicsConfig -> Context -> IO ()
printSearchTree h (bindings, scontext) = mapM_ f checkData
  where
    f (name, allowUnused, typeStr, _expected) = do
      let input = ExferenceInput
                    (readConstrainedType scontext typeStr)
                    (filter (\(x,_,_) -> x/="join") bindings)
                    scontext
                    allowUnused
                    256
                    (Just 256)
                    h
      let (_, tree, _) = last $ findExpressionsWithStats input
      let showf (total,processed,expression,_)
            = printf "%d (+%d): %s" processed
                                    (total-processed)
                                    (show expression)
      putStrLn $ drawTree $ fmap showf $ filterSearchTreeProcessedN 2 $ tree

-- TODO: remove duplication
pointfree :: String -> IO String
pointfree s = (!!1) <$> lines <$> readProcess "pointfree" ["--verbose", s] ""

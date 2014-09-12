{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Main
  ( main
  )
where



import Language.Haskell.Exference
import Language.Haskell.Exference.ExpressionToHaskellSrc
import Language.Haskell.Exference.BindingsFromHaskellSrc
import Language.Haskell.Exference.ContextFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.FunctionBinding

import Language.Haskell.Exference.ConstrainedType
import Language.Haskell.Exference.SimpleDict
import Language.Haskell.Exference.TypeClasses
import Language.Haskell.Exference.Expression
import Language.Haskell.Exference.ExferenceStats

import Control.DeepSeq

import System.Process

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( second, (***) )
import Control.Monad ( when, forM_, guard, forM, mplus, mzero )
import Data.List ( sortBy, find )
import Data.Ord ( comparing )
import Text.Printf
import Data.Maybe ( listToMaybe, fromMaybe, maybeToList )
import Data.Either ( lefts, rights )
import Control.Monad.Writer.Strict

import Language.Haskell.Exts.Syntax ( Module(..), Decl(..) )
import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , parseModule
                                    , ParseResult (..)
                                    , ParseMode (..) )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )

import Data.PPrint

import Debug.Hood.Observe



testInput :: [(String, Bool, String)]
testInput = 
  [ (,,) "showmap"    False "(Show b) => (a -> b) -> List a -> List String"
  , (,,) "ffbind"     False "(a -> t -> b) -> (t -> a) -> (t -> b)"
  , (,,) "join"       False "(Monad m) => m (m a) -> m a"
  , (,,) "fjoin"      False "(t -> (t -> a)) -> t -> a"
  , (,,) "zipThingy"  False "List a -> b -> List (Tuple a b)"
  , (,,) "stateRun"   True  "State a b -> a -> b"
  , (,,) "fst"        True  "Tuple a b -> a"
  , (,,) "ffst"       True  "(a -> Tuple b c) -> a -> b"
  , (,,) "snd"        True  "Tuple a b -> b"
  , (,,) "quad"       False "a -> Tuple (Tuple a a) (Tuple a a)"
  , (,,) "fswap"      False "(a -> Tuple b c) -> a -> Tuple c b"
  , (,,) "liftBlub"   False "Monad m => m a -> m b -> (a -> b -> m c) -> m c"
  , (,,) "stateBind"  False "State s a -> (a -> State s b) -> State s b"
  , (,,) "dbMaybe"    False "Maybe a -> Maybe (Tuple a a)"
  , (,,) "tupleShow"  False "Show a, Show b => Tuple a b -> String"
  , (,,) "FloatToInt" False "Float -> Int"
  , (,,) "longApp"    False "a -> b -> c -> (a -> b -> d) -> (a -> c -> e) -> (b -> c -> f) -> (d -> e -> f -> g) -> g"
  , (,,) "liftSBlub"  False "Monad m, Monad n => (List a -> b -> c) -> m (List (n a)) -> m (n b) -> m (n c)"
  , (,,) "liftSBlubS" False "Monad m => (List a -> b -> c) -> m (List (Maybe a)) -> m (Maybe b) -> m (Maybe c)"
  , (,,) "joinBlub"   False "Monad m => List Decl -> (Decl -> m (List FunctionBinding)) -> m (List FunctionBinding)"
  ]

expected :: [(String, Expression)]
expected =
  [ (,) "showmap"
    (ExpLambda 1
      (ExpLambda 2
        (ExpApply
          (ExpApply
            (ExpLit "fmap")
            (ExpLambda 6
              (ExpApply
                (ExpLit "show")
                (ExpApply (ExpVar 1) (ExpVar 6)))))
          (ExpVar 2))))
  , (,) "ffbind"
    (ExpLambda 1
      (ExpLambda 2
        (ExpLambda 3
          (ExpApply
            (ExpApply
              (ExpVar 1)
              (ExpApply (ExpVar 2) (ExpVar 3)))
            (ExpVar 3)))))
  , (,) "join"
    (ExpLambda 1
      (ExpApply
        (ExpApply (ExpLit "(>>=)") (ExpVar 1))
        (ExpLambda 5 (ExpVar 5))))
  , (,) "fjoin"
    (ExpLambda 1
      (ExpLambda 2
        (ExpApply (ExpApply (ExpVar 1) (ExpVar  2)) (ExpVar 2))))
  , (,) "zipThingy"
    (ExpLambda 1
      (ExpLambda 2
        (ExpApply
          (ExpApply
            (ExpLit "fmap")
            (ExpLambda 6
              (ExpApply
                (ExpApply (ExpLit "(,)") (ExpVar 6))
                (ExpVar 2))))
          (ExpVar 1))))
  , (,) "stateRun"
    (ExpLambda 1
      (ExpLambda 2
        (ExpLetMatch "State" [4] (ExpVar 1)
          (ExpLetMatch "(,)" [7,8]
            (ExpApply (ExpVar 4) (ExpVar 2))
            (ExpVar 7)))))
  , (,) "fst"
    (ExpLambda 1 (ExpLetMatch "(,)" [3,4] (ExpVar 1) (ExpVar 3)))
  , (,) "snd"
    (ExpLambda 1 (ExpLetMatch "(,)" [3,4] (ExpVar 1) (ExpVar 4)))
  , (,) "quad"
    (ExpLambda 1
      (ExpApply
        (ExpApply
          (ExpLit "(,)")
          (ExpApply (ExpApply (ExpLit "(,)") (ExpVar 1)) (ExpVar 1)))
        (ExpApply (ExpApply (ExpLit "(,)") (ExpVar 1)) (ExpVar 1))))
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
  , (,) "stateBind"
    (ExpLambda 1
      (ExpLambda 2
        (ExpLetMatch "State" [4] (ExpVar 1)
          (ExpApply
            (ExpLit "State")
            (ExpLambda 6
              (ExpLetMatch "(,)" [10,11]
                (ExpApply
                  (ExpVar 4)
                  (ExpVar 6))
                (ExpLetMatch "State" [14]
                  (ExpApply (ExpVar 2) (ExpVar 10))
                  (ExpApply (ExpVar 14) (ExpVar 11)))))))))
  , (,) "dbMaybe"
    (ExpLambda 1
      (ExpApply
        (ExpApply
          (ExpLit "fmap")
          (ExpLambda 5
            (ExpApply
              (ExpApply (ExpLit "(,)") (ExpVar 5))
              (ExpVar 5))))
        (ExpVar 1)))
  ]

checkOutput :: [( String
                , Expression
                , Maybe (Expression, Expression)
                , Maybe (Int, ExferenceStats)
                )]
checkOutput = do
  (iname, allowUnused, typeStr) <- testInput
  (ename, expr)    <- expected
  guard $ iname==ename
  let input = ExferenceInput
                (readConstrainedType defaultContext typeStr)
                bindings
                defaultContext
                allowUnused
  let r = findExpressions input
  let finder :: Int -> [(Expression, ExferenceStats)] -> Maybe (Int, ExferenceStats)
      finder n [] = Nothing
      finder n ((e, s):r) | e==expr = Just (n, s)
                          | otherwise = finder (n+1) r
      bestFound = findSortNExpressions 100 input
      firstAndBest :: Maybe (Expression, Expression)
      firstAndBest = do
        (f,_) <- listToMaybe r
        (b,_) <- listToMaybe bestFound
        return (f,b)
  return (iname, expr, firstAndBest, finder 0 r)
  


testOutput :: [[(Expression, ExferenceStats)]]
testOutput = map f testInput
  where
    input = ExferenceInput
    f (_, allowUnused, s) = takeFindSortNExpressions 5 10 $ ExferenceInput
                (readConstrainedType defaultContext s)
                bindings
                defaultContext
                allowUnused

testInOut = zip testInput testOutput

printAndStuff = mapM_ f testInOut
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

printStatistics = mapM_ f testInOut
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

printChecks :: IO ()
printChecks = mapM_ helper checkOutput
  where
    helper :: ( String
              , Expression
              , Maybe (Expression, Expression)
              , Maybe (Int, ExferenceStats))
           -> IO ()
    helper (name, _, Just(f,b), Just (0, ExferenceStats n d))
      | f==b = do putStrLn $ printf "%-10s: fine                                  %5d %8.2f" name n d
      | otherwise = do
      putStrLn $ printf "%-10s: expected solution first, but not best!" name
      putStrLn $ "  expected solution: " ++ show f
      putStrLn $ "  best solution:     " ++ show b
    helper (name, e, Just(f,_), Just (i, _)) = do
      putStrLn $ printf "%-10s: expected solution not first, but %d!" name i
      putStrLn $ "  first solution:    " ++ show f
      putStrLn $ "  expected solution: " ++ show e
    helper (name, e, Just(f,b), Nothing) = do
      putStrLn $ printf "%-10s: expected solution not found!" name
      putStrLn $ "  first solution was " ++ show f
      putStrLn $ "  best solution:     " ++ show b
      putStrLn $ "  expected solution: " ++ show e
    helper (name, _, Nothing, _) = do
      putStrLn $ printf "%-10s: no solutions found at all!" name 

testDictRatings :: [(String, Float)]
testDictRatings =
  [ (,) "maybe"    0.0
  , (,) "either"   0.0
  , (,) "curry"    0.0
  , (,) "uncurry"  0.0
  --, (,) "compare"  0.0
  --, (,) "minBound" 0.0
  --, (,) "maxBound" 0.0
  --, (,) ">>="      0.0
  --, (,) "pure"     0.0
  --, (,) "fmap"     0.0
  , (,) "mapM"     0.0
  , (,) "sequence" 0.0
  , (,) "foldl"    0.0
  , (,) "foldr"    0.0
  , (,) "concat"   0.0
  , (,) "zip"      0.0
  , (,) "zip3"     0.0
  , (,) "zipWith"  0.0
  , (,) "unzip"    0.0
  , (,) "unzip3"   0.0
  , (,) "repeat"   0.0
  , (,) "Just"     0.0
  --, (,) "&&"       0.0
  --, (,) "||"       0.0
  ]

compileDict :: [(String, Float)]
            -> [FunctionBinding]
            -> Either String [RatedFunctionBinding]
            -- function_not_found or all bindings
compileDict ratings binds = forM ratings $ \(name, rating) ->
  case find ((name==).fst) binds of
    Nothing -> Left name
    Just (_,t) -> Right (name, rating, t)

data A a = A Int

instance forall a . Show (A a) where
  show (A i) = show i

parseExternal :: [(ParseMode, String)]
              -> IO (Writer [String] ([FunctionBinding], StaticContext))
parseExternal l = do
  rawTuples <- mapM hRead l
  let eParsed = map hParse rawTuples
  {-
  let h :: Decl -> IO ()
      h i@(InstDecl _ _ _ _ _ _ _) = do
        pprint i >>= print
      h _ = return ()
  forM_ (rights eParsed) $ \(Module _ _ _ _ _ _ ds) ->
    forM_ ds h
  -}
  forM_ (rights eParsed) $ \m -> pprintTo 10000 m >>= print
  return $ do
    mapM_ (tell.return) $ lefts eParsed
    let mods = rights eParsed
    (,).join <$> (mapM hExtractBinds mods)
             <*> getContext mods
  where
    hRead :: (ParseMode, String) -> IO (ParseMode, String)
    hParse :: (ParseMode, String) -> Either String Module
    hExtractBinds :: Module -> Writer [String] [FunctionBinding]
    hRead (mode, s) = (,) mode <$> readFile s
    hParse (mode, content) = case parseModuleWithMode mode content of
      f@(ParseFailed _ _) -> Left $ show f
      ParseOk mod -> Right mod
    hExtractBinds mod = do
      let ebinds = getBindings defaultContext mod ++ getDataConss mod
      mapM_ (tell.return) $ lefts ebinds
      return $ rights ebinds

testBaseInput :: [(Bool, String)]
testBaseInput = [ (,) True  "GHCEnum"
                , (,) True  "GHCReal"
                , (,) True  "GHCShow"
                , (,) False "ControlMonad"
                , (,) False "DataEither"
                , (,) False "DataList"
                , (,) False "DataMaybe"
                , (,) False "DataTuple"
                , (,) False "DataOrd"
                , (,) False "GHCArr"
                , (,) False "GHCBase"
                , (,) False "GHCFloat"
                , (,) False "GHCList"
                , (,) False "GHCNum"
                , (,) False "GHCST"
                , (,) False "SystemIOError"
                , (,) False "SystemIO"
                , (,) False "TextRead"
                ]

testBaseInput' :: [(ParseMode, String)]
testBaseInput' = map h testBaseInput
  where
    h (shouldBangPattern, s) =
      let exts1 = (if shouldBangPattern then (BangPatterns:) else id)
                  [ UnboxedTuples
                  , TypeOperators
                  , MagicHash
                  , NPlusKPatterns
                  , ExplicitForAll
                  , ExistentialQuantification
                  , TypeFamilies
                  , PolyKinds
                  , DataKinds ]
          exts2 = map EnableExtension exts1
          mode = ParseMode (s++".hs")
                           Haskell2010
                           exts2
                           False
                           False
                           Nothing
          fname = "./BaseContext/preprocessed/"++s++".hs"
      in (mode, fname)

main = runO $ do
  --printAndStuff
  --printChecks
  --printStatistics
  let
    tryParse :: Bool -> String -> IO ()
    tryParse shouldBangPattern s = do
      content <- readFile $ "./BaseContext/preprocessed/"++s++".hs"
      let exts1 = (if shouldBangPattern then (BangPatterns:) else id)
                  [ UnboxedTuples
                  , TypeOperators
                  , MagicHash
                  , NPlusKPatterns
                  , ExplicitForAll
                  , ExistentialQuantification
                  , TypeFamilies
                  , PolyKinds
                  , DataKinds ]
          exts2 = map EnableExtension exts1
      case parseModuleWithMode (ParseMode (s++".hs")
                                          Haskell2010
                                          exts2
                                          False
                                          False
                                          Nothing
                               )
                               content of
        f@(ParseFailed _ _) -> do
          print f
        ParseOk mod -> do
          putStrLn s
          --mapM_ putStrLn $ map (either id show)
          --               $ getBindings defaultContext mod
          mapM_ putStrLn $ map (either id show)
                         $ getDataConss mod
  ((eSignatures, StaticContext clss insts), messages) <- runWriter <$> parseExternal testBaseInput'
  mapM_ print $ messages
  print $ compileDict testDictRatings $ eSignatures
  mapM_ print $ clss
  mapM_ print $ insts
  -- print $ parseConstrainedType defaultContext $ "(Show a) => [a] -> String"
  -- print $ inflateConstraints a b
  {-
  print $ constraintMatches testDynContext (badReadVar "y") (read "x")
  print $ constraintMatches testDynContext (badReadVar "x") (read "y")
  print $ constraintMatches testDynContext (badReadVar "w") (read "MyFoo")
  print $ constraintMatches testDynContext (badReadVar "w") (read "MyBar")
  print $ constraintMatches testDynContext (badReadVar "y") (read "MyFoo")
  print $ isProvable testDynContext [Constraint c_applicative [read "y"]]
  -}
  -- putStrLn $ show n ++ " total steps"
  {-
  let t :: HsType
      t = read "m a->( ( a->m b)->( m b))"
  print $ t
  -}

pointfree :: String -> IO String
pointfree s = (!!1) <$> lines <$> readProcess "pointfree" ["--verbose", s] ""

pointful :: String -> IO String
pointful s = (!!0) <$> lines <$> readProcess "pointful" [s] ""

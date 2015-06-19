{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.EnvironmentParser
  ( parseModules
  , parseModulesSimple
  , environmentFromModuleAndRatings
  , environmentFromPath
  , haskellSrcExtsParseMode
  , compileWithDict
  , ratingsFromFile
  )
where



import Language.Haskell.Exference
import Language.Haskell.Exference.ExpressionToHaskellSrc
import Language.Haskell.Exference.BindingsFromHaskellSrc
import Language.Haskell.Exference.ClassEnvFromHaskellSrc
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.FunctionDecl

import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.SimpleDict
import Language.Haskell.Exference.Core.Expression
import Language.Haskell.Exference.Core.ExferenceStats

import Control.DeepSeq

import System.Process

import Control.Applicative ( (<$>), (<*>), (<*) )
import Control.Arrow ( second, (***) )
import Control.Monad ( when, forM_, guard, forM, mplus, mzero )
import Data.List ( sortBy, find, isSuffixOf )
import Data.Ord ( comparing )
import Text.Printf
import Data.Maybe ( listToMaybe, fromMaybe, maybeToList )
import Data.Either ( lefts, rights )
import Control.Monad.Writer.Strict
import System.Directory ( getDirectoryContents )

import Language.Haskell.Exts.Syntax ( Module(..), Decl(..), ModuleName(..) )
import Language.Haskell.Exts.Parser ( parseModuleWithMode
                                    , parseModule
                                    , ParseResult (..)
                                    , ParseMode (..)
                                    , defaultParseMode )
import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )

import Control.Arrow ( first )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

import qualified Data.Map as M


builtInDecls :: [HsFunctionDecl]
builtInDecls =
  (Cons, (TypeArrow
            (TypeVar 0)
            (TypeArrow (TypeApp (TypeCons ListCon)
                                (TypeVar 0))
                       (TypeApp (TypeCons ListCon)
                                (TypeVar 0)))))
  : map (second $ unsafeReadType0)
    [ (,) (TupleCon 0) "Unit"
    , (,) (TupleCon 2) "a -> b -> (a, b)"
    , (,) (TupleCon 3) "a -> b -> c -> (a, b, c)"
    , (,) (TupleCon 4) "a -> b -> c -> d -> (a, b, c, d)"
    , (,) (TupleCon 5) "a -> b -> c -> d -> e -> (a, b, c, d, e)"
    , (,) (TupleCon 6) "a -> b -> c -> d -> e -> f -> (a, b, c, d, e, f)"
    , (,) (TupleCon 7) "a -> b -> c -> d -> e -> f -> g -> (a, b, c, d, e, f, g)"
    ]

builtInDeconstructors :: [DeconstructorBinding]
builtInDeconstructors = map helper ds
 where
  helper (t, xs) = ( unsafeReadType0 t
                   , xs
                   , False
                   )
  ds = [ (,) "(a, b)" [(TupleCon 2, [TypeVar 0, TypeVar 1])]
       , (,) "(a, b, c)" [(TupleCon 3, [TypeVar 0, TypeVar 1, TypeVar 2])]
       , (,) "(a, b, c, d)" [(TupleCon 4, [TypeVar 0, TypeVar 1, TypeVar 2, TypeVar 3])]
       , (,) "(a, b, c, d, e)" [(TupleCon 5, [TypeVar 0, TypeVar 1, TypeVar 2, TypeVar 3, TypeVar 4])]
       , (,) "(a, b, c, d, e, f)" [(TupleCon 6, [TypeVar 0, TypeVar 1, TypeVar 2, TypeVar 3, TypeVar 4, TypeVar 5])]
       , (,) "(a, b, c, d, e, f, g)" [(TupleCon 7, [TypeVar 0, TypeVar 1, TypeVar 2, TypeVar 3, TypeVar 4, TypeVar 5, TypeVar 6])]
       ]

-- | Takes a list of bindings, and a dictionary of desired
-- functions and their rating, and compiles a list of
-- RatedFunctionBindings.
-- 
-- If a function in the dictionary is not in the list of bindings,
-- Left is returned with the corresponding name.
--
-- Otherwise, the result is Right.
compileWithDict :: [(QualifiedName, Float)]
                -> [HsFunctionDecl]
                -> Either String [RatedHsFunctionDecl]
                -- function_not_found or all bindings
compileWithDict ratings binds = ratings `forM` \(name, rating) ->
  case find ((name==).fst) binds of
    Nothing    -> Left $ show name
    Just (_,t) -> Right (name, rating, t)

-- | input: a list of filenames for haskell modules and the
-- parsemode to use for it.
--
-- output: the environment extracted from these modules, wrapped
-- in a Writer that contains warnings/errors.
parseModules :: [(ParseMode, String)]
             -> IO (Writer
                      [String]
                      ( [HsFunctionDecl]
                      , [DeconstructorBinding]
                      , StaticClassEnv
                      , [QualifiedName] ))
parseModules l = do
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
  -- forM_ (rights eParsed) $ \m -> pprintTo 10000 m >>= print
  return $ do
    mapM_ (tell.return) $ lefts eParsed
    let mods = rights eParsed
    let ds = getDataTypes mods
    (cntxt@(StaticClassEnv clss insts), n_insts) <- getClassEnv ds mods
    -- TODO: try to exfere this stuff
    (decls, deconss) <- do
      stuff <- mapM (hExtractBinds cntxt ds) mods
      return $ concat *** concat $ unzip stuff
    let allValidNames = ds ++ (tclass_name <$> clss)
    let
      dataToBeChecked :: [(String, HsType)]
      dataToBeChecked =
           [ ("the instance data for " ++ show i, t)
           | insts' <- M.elems insts
           , i@(HsInstance _ _ ts) <- insts'
           , t <- ts]
        ++ [ ("the binding " ++ show n, t)
           | (n, t) <- decls]
    let
      check :: String -> HsType -> Writer [String] ()
      check s t = findInvalidNames allValidNames t `forM_` \n ->
        tell ["unknown binding '"++show n++"' used in " ++ s]
    dataToBeChecked `forM_` uncurry check
    tell ["got " ++ show (length clss) ++ " classes"]
    tell ["and " ++ show (n_insts) ++ " instances"]
    tell ["(-> " ++ show (length $ concat $ M.elems $ insts) ++ " instances after inflation)"]
    tell ["and " ++ show (length decls) ++ " function decls"]
    return $ ( builtInDecls++decls
             , builtInDeconstructors++deconss
             , cntxt
             , allValidNames
             )
  where
    hRead :: (ParseMode, String) -> IO (ParseMode, String)
    hRead (mode, s) = (,) mode <$> readFile s
    hParse :: (ParseMode, String) -> Either String Module
    hParse (mode, content) = case parseModuleWithMode mode content of
      f@(ParseFailed _ _) -> Left $ show f
      ParseOk modul       -> Right modul
    hExtractBinds :: StaticClassEnv
                  -> [QualifiedName]
                  -> Module
                  -> Writer [String] ([HsFunctionDecl], [DeconstructorBinding])
    hExtractBinds cntxt ds modul@(Module _ (ModuleName _mname) _ _ _ _ _) = do
      -- tell $ return $ mname
      let eFromData = getDataConss (sClassEnv_tclasses cntxt) ds [modul]
          eDecls = getDecls ds (sClassEnv_tclasses cntxt) [modul]
                 ++ getClassMethods (sClassEnv_tclasses cntxt) ds [modul]
      mapM_ (tell.return) $ lefts eFromData ++ lefts eDecls
      -- tell $ map show $ rights ebinds
      let (binds1s, deconss) = unzip $ rights eFromData
          binds2 = rights eDecls
      return $ ( concat binds1s ++ binds2, deconss )

-- | A simplified version of environmentFromModules where the input
-- is just one module, parsed with some default ParseMode;
-- the output is transformed so that all functionsbindings get
-- a rating of 0.0.
parseModulesSimple :: String
                   -> IO (Writer
                        [String]
                        ( [RatedHsFunctionDecl]
                        , [DeconstructorBinding]
                        , StaticClassEnv
                        , [QualifiedName] ))
parseModulesSimple s = (helper <$>)
                   <$> parseModules [(haskellSrcExtsParseMode s, s)]
 where
  addRating (a,b) = (a,0.0,b)
  helper (decls, deconss, cntxt, ds) = (addRating <$> decls, deconss, cntxt, ds)

ratingsFromFile :: String -> IO (Either String [(QualifiedName, Float)])
ratingsFromFile s = do
  content <- readFile s
  let
    parser =
      (many $ try $ do
        spaces
        name <- many1 (noneOf " ")
        _ <- space
        spaces
        _ <- char '='
        spaces
        minus <- optionMaybe $ char '-'
        a <- many1 digit
        b <- char '.'
        c <- many1 digit
        let qname = parseQualifiedName name
        case minus of
          Nothing -> return (qname, read $ a++b:c)
          Just _  -> return (qname, read $ '-':a++b:c))
      <* spaces
  return $ case runParser parser () "" content of
    Left e -> Left $ show e
    Right x -> Right x

-- TODO: add warnings for ratings not applied
environmentFromModuleAndRatings :: String
                                -> String
                                -> IO (Writer
                                    [String]
                                    ( [FunctionBinding]
                                    , [DeconstructorBinding]
                                    , StaticClassEnv
                                    , [QualifiedName] ))
environmentFromModuleAndRatings s1 s2 = do
  let exts1 = [ TypeOperators
              , ExplicitForAll
              , ExistentialQuantification
              , TypeFamilies
              , FunctionalDependencies
              , FlexibleContexts
              , MultiParamTypeClasses ]
      exts2 = map EnableExtension exts1
      mode = ParseMode (s1++".hs")
                       Haskell2010
                       exts2
                       False
                       False
                       Nothing
  w <- parseModules [(mode, s1)]
  r <- ratingsFromFile s2
  return $ do
    (decls, deconss, cntxt, ds) <- w
    case r of
      Left e -> do
        tell ["could not parse ratings!",e]
        return ([], [], cntxt, [])
      Right x -> do
        let f (a,b) = declToBinding
                    $ ( a
                      , fromMaybe 0.0 (lookup a x)
                      , b
                      )
        return $ (map f decls, deconss, cntxt, ds)

environmentFromPath :: FilePath
                    -> IO (Writer
                         [String]
                         ( [FunctionBinding]
                         , [DeconstructorBinding]
                         , StaticClassEnv
                         , [QualifiedName] ))
environmentFromPath p = do
  files <- getDirectoryContents p
  let modules = ((p ++ "/")++) <$> filter (".hs" `isSuffixOf`) files
  let ratings = ((p ++ "/")++) <$> filter (".ratings" `isSuffixOf`) files
  w <- parseModules [ (mode, m)
                    | m <- modules
                    , let mode = haskellSrcExtsParseMode m]
  rResult <- ratingsFromFile `mapM` ratings
  let rs = [x | Right xs <- rResult, x <- xs]
  return $ do
    (decls, deconss, cntxt, dts) <- w
    sequence_ $ do
      Left err <- rResult
      return $ tell ["could not parse rating file", err]
    sequence_ $ do
      r <- rs
      guard $ show (fst r) `notElem` (show . fst <$> decls)
      return $ tell ["rating could not be applied: " ++ show (fst r)]
    let f (a,b) = declToBinding
                $ ( a
                  , fromMaybe 0.0 (lookup (show a) (first show <$> rs))
                  , b
                  )
    return $ (map f decls, deconss, cntxt, dts)

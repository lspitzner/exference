{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}

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
import Data.Maybe ( listToMaybe, fromMaybe, maybeToList, catMaybes )
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
import Control.Monad.Trans.MultiRWS
import Data.HList.ContainsType

import Language.Haskell.Exference.Core.TypeUtils

import qualified Data.Map as M
import qualified Data.IntMap as IntMap


builtInDeclsM :: ( ContainsType QNameIndex s
                 , Monad m
                 )
              => MultiRWST r w s m [HsFunctionDecl]
builtInDeclsM = do
  cons <- getOrCreateQNameId Cons
  listCon <- getOrCreateQNameId ListCon
  let consType = (cons, (TypeArrow
            (TypeVar 0)
            (TypeArrow (TypeApp (TypeCons listCon)
                                (TypeVar 0))
                       (TypeApp (TypeCons listCon)
                                (TypeVar 0)))))
  tupleConss <- mapM (\(a,b) -> [(x,y) | x <- getOrCreateQNameId a
                                       , y <- unsafeReadType0 b])
    [ (,) (TupleCon 0) "Unit"
    , (,) (TupleCon 2) "a -> b -> (a, b)"
    , (,) (TupleCon 3) "a -> b -> c -> (a, b, c)"
    , (,) (TupleCon 4) "a -> b -> c -> d -> (a, b, c, d)"
    , (,) (TupleCon 5) "a -> b -> c -> d -> e -> (a, b, c, d, e)"
    , (,) (TupleCon 6) "a -> b -> c -> d -> e -> f -> (a, b, c, d, e, f)"
    , (,) (TupleCon 7) "a -> b -> c -> d -> e -> f -> g -> (a, b, c, d, e, f, g)"
    ]
  return $ consType : tupleConss

builtInDeconstructorsM :: ( ContainsType QNameIndex s
                          , Monad m
                          )
                       => MultiRWST r w s m [DeconstructorBinding]
builtInDeconstructorsM = mapM helper ds
 where
  helper (t, xs) = [ (x,y,False)
                   | x <- unsafeReadType0 t
                   , y <- xs `forM` \(a,b) -> [ (v,b)
                                              | v <- getOrCreateQNameId a
                                              ]
                   ]
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
compileWithDict :: ( MonadMultiState QNameIndex m )
                => [(QualifiedName, Float)]
                -> [HsFunctionDecl]
                -> m (Either String [RatedHsFunctionDecl])
                -- function_not_found or all bindings
compileWithDict ratings binds = do
  rat <- ratings `forM` \(qname, r) -> [ (qnid, r)
                                       | qnid <- getOrCreateQNameId qname]
  return $ rat `forM` \(name, rating) ->
    case find ((name==).fst) binds of
      Nothing    -> Left $ show name
      Just (_,t) -> Right (name, rating, t)

-- | input: a list of filenames for haskell modules and the
-- parsemode to use for it.
--
-- output: the environment extracted from these modules, wrapped
-- in a Writer that contains warnings/errors.
parseModules :: forall m r w s
              . ( m ~ MultiRWST r w s IO
                , ContainsType [String] w
                , ContainsType QNameIndex s
                )
             => [(ParseMode, String)]
             -> m
                  ( [HsFunctionDecl]
                  , [DeconstructorBinding]
                  , StaticClassEnv
                  , [QualifiedName] )
parseModules l = do
  rawTuples <- lift $ mapM hRead l
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
  mapM_ (mTell . (:[])) $ lefts eParsed
  let mods = rights eParsed
  let ds = getDataTypes mods
  (cntxt@(StaticClassEnv clss insts), n_insts) <- getClassEnv ds mods
  -- TODO: try to exfere this stuff
  (decls, deconss) <- do
    stuff <- mapM (hExtractBinds cntxt ds) mods
    return $ concat *** concat $ unzip stuff
  clssNames <- catMaybes <$> mapM (lookupQNameId . tclass_name) clss
  let allValidNames = ds ++ clssNames
  let
    dataToBeChecked :: [(String, HsType)]
    dataToBeChecked =
         [ ("the instance data for " ++ show i, t)
         | insts' <- IntMap.elems insts
         , i@(HsInstance _ _ ts) <- insts'
         , t <- ts]
      ++ [ ("the binding " ++ show n, t)
         | (n, t) <- decls]
  let
    check :: String -> HsType -> m ()
    check s t = do
      names <- findInvalidNames allValidNames t
      names `forM_` \n ->
        mTell ["unknown binding '"++show n++"' used in " ++ s]
  dataToBeChecked `forM_` uncurry check
  mTell ["got " ++ show (length clss) ++ " classes"]
  mTell ["and " ++ show (n_insts) ++ " instances"]
  mTell ["(-> " ++ show (length $ concat $ IntMap.elems $ insts) ++ " instances after inflation)"]
  mTell ["and " ++ show (length decls) ++ " function decls"]
  builtInDecls          <- builtInDeclsM
  builtInDeconstructors <- builtInDeconstructorsM
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
                  -> m ([HsFunctionDecl], [DeconstructorBinding]) 
    hExtractBinds cntxt ds modul@(Module _ (ModuleName _mname) _ _ _ _ _) = do
      -- tell $ return $ mname
      eFromData <- getDataConss (sClassEnv_tclasses cntxt) ds [modul]
      eDecls <- (++)
        <$> getDecls ds (sClassEnv_tclasses cntxt) [modul]
        <*> getClassMethods (sClassEnv_tclasses cntxt) ds [modul]
      mapM_ (mTell . (:[])) $ lefts eFromData ++ lefts eDecls
      -- tell $ map show $ rights ebinds
      let (binds1s, deconss) = unzip $ rights eFromData
          binds2 = rights eDecls
      return $ ( concat binds1s ++ binds2, deconss )

-- | A simplified version of environmentFromModules where the input
-- is just one module, parsed with some default ParseMode;
-- the output is transformed so that all functionsbindings get
-- a rating of 0.0.
parseModulesSimple :: ( ContainsType [String] w
                      , ContainsType QNameIndex s
                      )
                   => String
                   -> MultiRWST r w s IO
                        ( [RatedHsFunctionDecl]
                        , [DeconstructorBinding]
                        , StaticClassEnv
                        , [QualifiedName] )
parseModulesSimple s = helper
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
environmentFromModuleAndRatings :: ( ContainsType [String] w
                                   , ContainsType QNameIndex s
                                   )
                                => String
                                -> String
                                -> MultiRWST r w s IO
                                    ( [FunctionBinding]
                                    , [DeconstructorBinding]
                                    , StaticClassEnv
                                    , [QualifiedName] )
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
  (decls, deconss, cntxt, ds) <- parseModules [(mode, s1)]
  r <- lift $ ratingsFromFile s2
  case r of
    Left e -> do
      mTell ["could not parse ratings!",e]
      return ([], [], cntxt, [])
    Right ratingsRaw -> do
      ratings <- ratingsRaw `forM` \(qname, rval) ->
            [ (qnid, rval)
            | qnid <- getOrCreateQNameId qname]
      let f (a,b) = declToBinding
                  $ ( a
                    , fromMaybe 0.0 (lookup a ratings)
                    , b
                    )
      return $ (map f decls, deconss, cntxt, ds)


environmentFromPath :: ( ContainsType [String] w
                       , ContainsType QNameIndex s
                       )
                    => FilePath
                    -> MultiRWST r w s IO
                         ( [FunctionBinding]
                         , [DeconstructorBinding]
                         , StaticClassEnv
                         , [QualifiedName] )
environmentFromPath p = do
  files <- lift $ getDirectoryContents p
  let modules = ((p ++ "/")++) <$> filter (".hs" `isSuffixOf`) files
  let ratings = ((p ++ "/")++) <$> filter (".ratings" `isSuffixOf`) files
  (decls, deconss, cntxt, dts) <- parseModules
    [ (mode, m)
    | m <- modules
    , let mode = haskellSrcExtsParseMode m]
  rResult <- lift $ ratingsFromFile `mapM` ratings
  let rs = [x | Right xs <- rResult, x <- xs]
  sequence_ $ do
    Left err <- rResult
    return $ mTell ["could not parse rating file", err]
  (rs' :: [(QNameId, Float)]) <- fmap join $ sequence $ do
    (rName, rVal) <- rs
    return $ do
      dIds <- fmap join $ sequence $ do
        (dId, _) <- decls
        return $ do
          dName <- lookupQNameId dId
          return $ do
            guard (Just (show rName) == (show <$> dName))
            return (dId, rVal)
      case dIds of
        [] -> do
          mTell ["rating could not be applied: " ++ show rName]
          return []
        [x] ->
          return [x]
        _ -> do
          mTell ["duplicate function: " ++ show rName]
          return []
  let f (a,b) = declToBinding
              $ ( a
                , fromMaybe 0.0 (lookup a rs')
                , b
                )
  return $ (map f decls, deconss, cntxt, dts)

{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.ClassEnvFromHaskellSrc
  ( getClassEnv
  )
where



import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils

import Language.Haskell.Exference.TypeFromHaskellSrc

import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as LazyMap
import Control.Monad.State.Strict
import Control.Monad.Trans.Either
import Control.Monad.Writer.Strict

import Control.Applicative ( (<$>), (<*>) )

import Data.Maybe ( fromMaybe )
import Data.Either ( lefts, rights )
import Data.List ( find )



-- | returns the environment and the number of class instances
--   found (before inflating the instances). The number of
--   classes can be derived from the other output.
--   The count may be used to inform the user (post-inflation count
--   would be bad for that purpose.)
getClassEnv :: [QualifiedName]
            -> [Module]
            -> Writer [String] (StaticClassEnv, Int)
getClassEnv ds ms = do
  let etcs = getTypeClasses ds ms
  mapM_ (tell.return) $ lefts etcs
  let tcs = rights etcs
  let einsts = getInstances tcs ds ms
  mapM_ (tell.return) $ lefts einsts
  let insts_uninflated = rights einsts
  let insts = inflateInstances insts_uninflated
  return $ (mkStaticClassEnv tcs insts, length insts_uninflated)

type RawMap = M.Map QualifiedName (ModuleName, Context, [TyVarBind])

getTypeClasses :: [QualifiedName] -> [Module] -> [Either String HsTypeClass]
getTypeClasses ds ms = let
  mdecls = [ (moduleName, d)
           | (Module _ moduleName _ _ _ _ decls) <- ms
           , d <- decls
           ]
  rawMap :: RawMap
  rawMap = M.fromList
    $ [ (convertModuleName moduleName name, (moduleName, context, vars))
      | ( moduleName
        , ClassDecl _loc context name vars _fdeps _cdecls
        ) <- mdecls
      ]
  helper :: QualifiedName
         -> (ModuleName, Context, [TyVarBind])
         -> Either String HsTypeClass
  helper name (moduleName, assts, vars) = let
    sAction :: ConversionMonad HsTypeClass
    sAction = do -- State s (Either String)
      varIds <- mapM tyVarTransform vars
      constrs <- mapM
        (constrTransform (Just moduleName) ds $
          \str -> fromMaybe (Right unknownTypeClass)
                $ LazyMap.lookup str resultMap)
        assts
      return $ HsTypeClass name varIds constrs
      --either (Left . (("class "++name++": ")++)) Right
    in evalState (runEitherT sAction) (0, M.empty)
  resultMap :: LazyMap.Map QualifiedName (Either String HsTypeClass)
    -- CARE: DONT USE STRICT METHODS ON THIS MAP
    --       (COMPILER WONT COMPLAIN)
  resultMap = LazyMap.mapWithKey helper rawMap
  in LazyMap.elems resultMap

getInstances :: [HsTypeClass]
             -> [QualifiedName]
             -> [Module]
             -> [Either String HsInstance]
getInstances tcs ds ms = do
  Module _ mn _ _ _ _ decls <- ms
  InstDecl _ _ _vars cntxt qname tps _ <- decls
    -- vars would be the forall binds in
    -- > instance forall a . Show (Foo a) where [..]
    -- which we can ignore, right?
  let name = convertQName (Just mn) ds qname
  let instClass = maybe (Left $ "unknown type class: "++show name) Right
                $ find ((name==).tclass_name) tcs
  let
    sAction :: ConversionMonad HsInstance
    sAction = do
      -- varIds <- mapM tyVarTransform vars
      constrs <- mapM
        (constrTransform (Just mn) ds $
          \str -> maybe (Left $ "unknown type class: "++show str) Right
                $ find ((str==).tclass_name) tcs)
        cntxt
      rtps <- convertTypeInternal tcs (Just mn) ds `mapM` tps
      ic <- hoistEither instClass
      return $ HsInstance constrs ic rtps
      -- either (Left . (("instance for "++name++": ")++)) Right
  return $ evalState (runEitherT sAction) (0, M.empty)

constrTransform :: Maybe ModuleName
                -> [QualifiedName]
                -> (QualifiedName -> Either String HsTypeClass)
                -> Asst
                -> ConversionMonad HsConstraint
constrTransform mn ds tcLookupF (ClassA qname types)
  | ctypes <- convertTypeInternal [] mn ds `mapM` types
  , constrClass <- tcLookupF $ convertQName mn ds qname
  = HsConstraint <$> hoistEither constrClass <*> ctypes
constrTransform mn ds tcLookupF (ParenA c) = constrTransform mn ds tcLookupF c
constrTransform _ _ _ c = left $ "unknown HsConstraint: " ++ show c

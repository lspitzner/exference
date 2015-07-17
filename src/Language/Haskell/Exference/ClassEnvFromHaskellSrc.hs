{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonadComprehensions #-}

module Language.Haskell.Exference.ClassEnvFromHaskellSrc
  ( getClassEnv
  )
where



import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils

import Language.Haskell.Exference.TypeFromHaskellSrc

import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as LazyMap
import Control.Monad.State.Strict
import Control.Monad.Trans.Either
import Control.Monad.Writer.Strict

import Control.Applicative ( (<$>), (<*>), Applicative )

import Data.Maybe ( fromMaybe )
import Data.Either ( lefts, rights )
import Data.List ( find )

import Control.Monad.Trans.MultiRWS
import Data.HList.ContainsType

import Debug.Trace



-- | returns the environment and the number of class instances
--   found (before inflating the instances). The number of
--   classes can be derived from the other output.
--   The count may be used to inform the user (post-inflation count
--   would be bad for that purpose.)
getClassEnv :: ( ContainsType [String] w
               , ContainsType QNameIndex s
               , MonadMultiState QNameIndex (MultiRWST r w s m)
               , MonadFix m
               , Applicative m
               )
            => [QualifiedName]
            -> TypeDeclMap
            -> [Module]
            -> MultiRWST r w s m (StaticClassEnv, Int)
getClassEnv ds tDeclMap ms = do
  etcs <- getTypeClasses ds tDeclMap ms
  mapM_ (mTell . (:[])) $ lefts etcs
  let tcs = rights etcs
  einsts <- getInstances tcs ds tDeclMap ms
  mapM_ (mTell . (:[])) $ lefts einsts
  let insts_uninflated = rights einsts
  let insts = inflateInstances insts_uninflated
  return (mkStaticClassEnv tcs insts, length insts_uninflated)

type TempAsst = (QNameId, [HsType])

getTypeClasses :: forall m r w s m0
                . ( ContainsType QNameIndex s
                  , MonadFix m0
                  , Applicative m0
                  , m ~ MultiRWST r w s m0
                  )
               => [QualifiedName]
               -> TypeDeclMap
               -> [Module]
               -> m [Either String HsTypeClass]
getTypeClasses ds tDeclMap ms = do
  let mdecls = [ (moduleName, d)
               | (Module _ moduleName _ _ _ _ decls) <- ms
               , d <- decls
               ]
  (rawMap :: M.Map QNameId (ModuleName, Context, [TyVarBind]))
    <- fmap M.fromList $ sequence $ do
      ( moduleName
       ,ClassDecl _loc context name vars _fdeps _cdecls
       ) <- mdecls
      return $ do
        qnid <- getOrCreateQNameId $ convertModuleName moduleName name
        return (qnid, (moduleName, context, vars))
  (secondMap :: M.Map QNameId (Either String ([TempAsst], [TVarId])))
    <- flip traverse rawMap
      $ \(moduleName, assts, vars) -> withMultiStateA (ConvData 0 M.empty) $ runEitherT
        [ (tempAssts, vars')
        | vars' <- mapM tyVarTransform vars
        , let convF (ClassA qname types) =
                [ (qnid, ctypes)
                | qnid <- getOrCreateQNameId (convertQName (Just moduleName)
                                                           ds
                                                           qname)
                , ctypes <- types `forM` convertTypeInternal []
                                                             (Just moduleName)
                                                             ds
                                                             tDeclMap
                ]
              convF (ParenA c)           = convF c
              convF c                    = left
                                         $ "unknown HsConstraint: " ++ show c
        , tempAssts <- mapM convF assts
        ]
  unknown <- unknownTypeClass
  let
    helper :: QNameId
           -> Either String ([TempAsst], [TVarId])
           -> Either String HsTypeClass
    helper _ (Left e) = Left e
    helper qnid (Right (tempAssts, tVarIds)) = HsTypeClass qnid tVarIds
                                             <$> mapM h tempAssts
      where
        h :: TempAsst -> Either String HsConstraint
        h (cQnid, types) = case LazyMap.lookup cQnid resultMap of
            Nothing -> Right $ HsConstraint unknown types
            Just (Left e)   -> Left e
            Just (Right tc) -> Right $ HsConstraint tc types
    resultMap :: LazyMap.Map QNameId (Either String HsTypeClass)
      -- CARE: DONT USE STRICT METHODS ON THIS MAP
      --       (COMPILER WONT COMPLAIN)
    resultMap = LazyMap.mapWithKey helper secondMap
  return $ LazyMap.elems $ resultMap

getInstances :: forall m m0 r w s
              . ( ContainsType QNameIndex s
                , m ~ MultiRWST r w s m0
                , Monad m0
                )
             => [HsTypeClass]
             -> [QualifiedName]
             -> TypeDeclMap
             -> [Module]
             -> m [Either String HsInstance]
getInstances tcs ds tDeclMap ms = sequence $ do
  Module _ mn _ _ _ _ decls <- ms
  InstDecl _ _ _vars cntxt qname tps _ <- decls
    -- vars would be the forall binds in
    -- > instance forall a . Show (Foo a) where [..]
    -- which we can ignore, right?
  let name = convertQName (Just mn) ds qname
  return $ do
    qnid <- getOrCreateQNameId name
    let instClass = maybe (Left $ "unknown type class: "++show name) Right
                  $ find ((qnid==).tclass_name) tcs
    let
      sAction :: forall m1
               . ( MonadMultiState QNameIndex m1
                 , MonadMultiState ConvData m1
                 )
              => EitherT String m1 HsInstance
      sAction = do
        -- varIds <- mapM tyVarTransform vars
        constrs <- cntxt `forM` \asst ->
          constrTransform
            (Just mn)
            ds
            tDeclMap
            (\str -> find ((str==).tclass_name) tcs)
            asst
        rtps <- convertTypeInternal tcs (Just mn) ds tDeclMap `mapM` tps
        ic <- hoistEither instClass
        return $ HsInstance constrs ic rtps
        -- either (Left . (("instance for "++name++": ")++)) Right
    withMultiStateA (ConvData 0 M.empty) $ runEitherT sAction

constrTransform :: ( MonadMultiState QNameIndex m
                   , MonadMultiState ConvData m
                   )
                => Maybe ModuleName
                -> [QualifiedName]
                -> TypeDeclMap
                -> (QNameId -> Maybe HsTypeClass)
                -> Asst
                -> EitherT String m HsConstraint
constrTransform mn ds tDeclMap tcLookupF (ClassA qname types) = do
  let ctypes = convertTypeInternal [] mn ds tDeclMap `mapM` types
  let qn = convertQName mn ds qname
  qnid <- getOrCreateQNameId qn
  case tcLookupF qnid of
    Nothing -> left $ "unknown type class: " ++ show qn
    Just tc -> HsConstraint tc <$> ctypes
constrTransform mn ds tDeclMap tcLookupF (ParenA c) = constrTransform mn ds tDeclMap tcLookupF c
constrTransform _ _ _ _ c = left $ "unknown HsConstraint: " ++ show c

{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.ContextFromHaskellSrc
  ( getContext
  )
where



import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exference.FunctionBinding
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.ConstrainedType
import Language.Haskell.Exference.Type
import Language.Haskell.Exference.TypeClasses

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



getContext :: [Module] -> Writer [String] StaticContext
getContext ms = do
  let etcs = getTypeClasses ms
  mapM_ (tell.return) $ lefts etcs
  let tcs = rights etcs
  let einsts = getInstances tcs ms
  mapM_ (tell.return) $ lefts einsts
  let insts = rights einsts
  return $ StaticContext tcs insts

type RawMap = M.Map String (Context, [TyVarBind])

getTypeClasses :: [Module] -> [Either String HsTypeClass]
getTypeClasses ms = let
  decls = concatMap (\(Module _ _ _ _ _ _ ds) -> ds) ms
  rawMap :: RawMap
  rawMap = M.fromList
    $ [ (hsNameToString name,(context, vars))
      | (ClassDecl _loc context name vars _fdeps _cdecls) <- decls]
  helper :: String
         -> (Context, [TyVarBind])
         -> Either String HsTypeClass
  helper name (assts, vars) = let
    sAction :: ConversionMonad HsTypeClass
    sAction = do -- State s (Either String)
      varIds <- mapM tyVarTransform vars
      constrs <- mapM
        (constrTransform $
          \str -> fromMaybe (Right unknownTypeClass)
                $ LazyMap.lookup str resultMap)
        assts
      return $ HsTypeClass name varIds constrs
      --either (Left . (("class "++name++": ")++)) Right
    in evalState (runEitherT sAction) (0, M.empty)
  resultMap :: LazyMap.Map String (Either String HsTypeClass)
    -- CARE: DONT USE STRICT METHODS ON THIS MAP
    --       (COMPILER WONT COMPLAIN)
  resultMap = LazyMap.mapWithKey helper rawMap
  in LazyMap.elems resultMap

getInstances :: [HsTypeClass] -> [Module] -> [Either String HsInstance]
getInstances tcs ms = do
  Module _ _ _ _ _ _ decls <- ms
  InstDecl _ _ _vars cntxt qname tps _ <- decls
    -- vars would be the forall binds in
    -- > instance forall a . Show (Foo a) where [..]
    -- which we can ignore, right?
  let name = hsQNameToString qname
  let instClass = maybe (Left $ "unknown type class: "++name) Right
                $ find ((name==).tclass_name) tcs
  let
    sAction :: ConversionMonad HsInstance
    sAction = do
      -- varIds <- mapM tyVarTransform vars
      constrs <- mapM
        (constrTransform $
          \str -> maybe (Left $ "unknown type class: "++str) Right
                $ find ((str==).tclass_name) tcs)
        cntxt
      rtps <- mapM convertTypeInternal tps
      ic <- hoistEither instClass
      return $ HsInstance constrs ic rtps
      -- either (Left . (("instance for "++name++": ")++)) Right
  return $ evalState (runEitherT sAction) (0, M.empty)

tyVarTransform :: TyVarBind
               -> ConversionMonad TVarId
tyVarTransform (KindedVar _ _) = left $ "KindedVar"
tyVarTransform (UnkindedVar n) = getVar n

constrTransform :: (String -> Either String HsTypeClass)
                -> Asst
                -> ConversionMonad Constraint
constrTransform tcLookupF (ClassA qname types)
  | ctypes <- mapM convertTypeInternal types
  , constrClass <- tcLookupF $ hsQNameToString qname
  -- 
  = Constraint <$> hoistEither constrClass <*> ctypes
constrTransform tcLookupF (ParenA c) = constrTransform tcLookupF c
constrTransform _ c = left $ "unknown constraint: " ++ show c

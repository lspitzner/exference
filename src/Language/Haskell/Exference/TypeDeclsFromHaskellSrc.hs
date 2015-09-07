{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference.TypeDeclsFromHaskellSrc
  ( HsTypeDecl (..)
  , TypeDeclMap
  , getTypeDecls
  , convertType
  , convertTypeInternal
  , parseType
  , unsafeReadType
  , unsafeReadType0
  )
where



import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.TypeFromHaskellSrc

import Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.Parser as P

import Control.Monad.Trans.MultiRWS
import Data.HList.ContainsType

import Control.Monad.Trans.Either ( runEitherT
                                  , mapEitherT
                                  , EitherT(..)
                                  , hoistEither
                                  , left
                                  )

import Control.Monad ( forM, join, liftM )
import Data.Either ( lefts, rights )
import Data.Bifunctor ( bimap )

import Data.Map ( Map )
import Data.IntMap ( IntMap )
import qualified Data.Map as M
import qualified Data.IntMap as IntMap



data HsTypeDecl = HsTypeDecl
  { tdecl_name :: QNameId
  , tdecl_params :: [TVarId]
  , tdecl_result :: HsType
  } deriving Show -- (Data, Show, Generic, Typeable)

type TypeDeclMap = IntMap HsTypeDecl

applyTypeDecls :: IntMap (Either String HsTypeDecl)
               -> HsType 
               -> Either String HsType
applyTypeDecls m = go
 where
  go (TypeVar i)      = Right $ TypeVar i
  go (TypeConstant i) = Right $ TypeConstant i
  go t@(TypeCons _)  = goApp [] t
  go (TypeArrow t1 t2) = [ TypeArrow t1' t2'
                         | t1' <- go t1
                         , t2' <- go t2
                         ]
  go (TypeApp l r) = goApp [r] l
  go (TypeForall vars constrs t) = TypeForall vars constrs `liftM` go t
  goApp rs (TypeApp l r)      = goApp (r:rs) l
  goApp rs (TypeCons qnId)    = case IntMap.lookup qnId m of
    Nothing                  -> foldl TypeApp (TypeCons qnId) `liftM` mapM go rs
    Just (Left _)            -> Right $ TypeCons qnId -- no need to show the
                                   -- same error multiple times, or is there?
    Just (Right (HsTypeDecl _ vs t))
                             | i <- length vs
                             , i <= length rs
                             -> [ foldl TypeApp substituted pUnchanged
                                | rs' <- mapM go rs
                                , let pAffected = take i rs'
                                , let pUnchanged = drop i rs'
                                , let substs = IntMap.fromList $ zip vs pAffected
                                , let substituted = snd $ applySubsts substs t
                                ]
    _                        -> Left $ "wrong number of parameters for type declaration " ++ show qnId
  goApp rs l               = foldl1 TypeApp `liftM` mapM go (l:rs)

getTypeDecls :: ( ContainsType QNameIndex s
                , Monad m
                )
             => [QualifiedName]
             -> [Module]
             -> MultiRWST r w s m [Either String HsTypeDecl]
getTypeDecls ds modules = do
  rawList <- sequence $ do
    Module _loc mn _pragma _warning _mexp _imp decls <- modules
    TypeDecl _loc name rawVars rawTy <- decls
    return $ liftM (bimap (("when parsing type declaration "++show name++": ")++) id)
           $ runEitherT
           $ do
      (ty, tyVarIndex) <- convertTypeNoDecl [] (Just mn) ds rawTy
      let qname = convertModuleName mn name
      qNameId <- getOrCreateQNameId qname
      -- the 1000 is arbitrary, but it should not be used anyway.
      -- no new type variables should appear on the left hand side.
      vars <- mapEitherT (withMultiStateA (ConvData 1000 tyVarIndex)) $ rawVars `forM` tyVarTransform
      return $ HsTypeDecl qNameId vars ty
  let converter (HsTypeDecl n vs t) = HsTypeDecl n vs `liftM` applyTypeDecls resultMap t
      resultMap :: IntMap (Either String HsTypeDecl)
      resultMap = IntMap.map converter
                $ IntMap.fromList
                $ map (\x -> (tdecl_name x, x))
                $ rights rawList
  return $ [ e | e@(Left _) <- rawList ] ++ IntMap.elems resultMap

convertType :: ( ContainsType QNameIndex s
               , Monad m
               )
            => [HsTypeClass]
            -> Maybe ModuleName
            -> [QualifiedName]
            -> TypeDeclMap
            -> Type
            -> EitherT String (MultiRWST r w s m) (HsType, TypeVarIndex)
convertType tcs mn ds declMap t = do
  (ty, index) <- convertTypeNoDecl tcs mn ds t
  ty' <- hoistEither $ applyTypeDecls (IntMap.map Right declMap) ty
  return $ (ty', index)

convertTypeInternal :: ( MonadMultiState QNameIndex m
                       , MonadMultiState ConvData m
                       )
                    => [HsTypeClass]
                    -> Maybe ModuleName -- default (for unqualified stuff)
                      -- Nothing uses a broad search for lookups
                    -> [QualifiedName] -- list of fully qualified data types
                                         -- (to keep things unique)
                    -> TypeDeclMap
                    -> Type
                    -> EitherT String m HsType
convertTypeInternal tcs defModuleName ds declMap t = do
  ty <- convertTypeNoDeclInternal tcs defModuleName ds t
  ty' <- hoistEither $ applyTypeDecls (IntMap.map Right declMap) ty
  return $ ty'

parseType :: ( ContainsType QNameIndex s
             , Monad m
             )
          => [HsTypeClass]
          -> Maybe ModuleName
          -> [QualifiedName]
          -> TypeDeclMap
          -> P.ParseMode
          -> String
          -> EitherT String (MultiRWST r w s m) (HsType, TypeVarIndex)
parseType tcs mn ds tDeclMap m s = case P.parseTypeWithMode m s of
  f@(P.ParseFailed _ _) -> left $ show f
  P.ParseOk t           -> convertType tcs mn ds tDeclMap t

unsafeReadType :: ( ContainsType QNameIndex s
                  , Monad m
                  )
               => [HsTypeClass]
               -> [QualifiedName]
               -> TypeDeclMap
               -> String
               -> MultiRWST r w s m HsType
unsafeReadType tcs ds tDeclMap s = do
  parseRes <- runEitherT $ parseType tcs Nothing ds tDeclMap (haskellSrcExtsParseMode "type") s
  return $ case parseRes of
    Left _ -> error $ "unsafeReadType: could not parse type: " ++ s
    Right (t, _) -> t

unsafeReadType0 :: ( ContainsType QNameIndex s
                   , Monad m
                   )
                => String
                -> MultiRWST r w s m HsType
unsafeReadType0 s = do
  parseRes <- runEitherT $ parseType [] Nothing [] (IntMap.empty) (haskellSrcExtsParseMode "type") s
  return $ case parseRes of
    Left _ -> error $ "unsafeReadType: could not parse type: " ++ s
    Right (t, _) -> t

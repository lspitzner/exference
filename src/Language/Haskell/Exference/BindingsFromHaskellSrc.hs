{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions #-}

module Language.Haskell.Exference.BindingsFromHaskellSrc
  ( getDecls
  , declToBinding
  , getDataConss
  , getClassMethods
  , getDataTypes
  )
where



import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exference.Core.FunctionBinding
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc
import Language.Haskell.Exference.Core.Types
import Language.Haskell.Exference.Core.TypeUtils
import Language.Haskell.Exference.FunctionDecl

import Control.Applicative ( (<$>), (<*>) )

import Control.Monad ( join )
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.List ( find )

import Control.Monad.Trans.MultiRWS
import Data.HList.ContainsType

import Debug.Trace



getDecls
  :: (Monad m, Functor m)
  => [QualifiedName]
  -> [HsTypeClass]
  -> TypeDeclMap
  -> [Module]
  -> MultiRWST r w s m [Either String HsFunctionDecl]
getDecls ds tcs tDeclMap modules = fmap (>>= either (return.Left) (map Right))
                                $ sequence
                                $ do
  Module _loc mn _pragma _warning _mexp _imp decls <- modules
  d <- decls
  return $ runEitherT $ transformDecl tcs ds mn tDeclMap d

transformDecl
  :: (Monad m, Functor m)
  => [HsTypeClass]
  -> [QualifiedName]
  -> ModuleName
  -> TypeDeclMap
  -> Decl
  -> EitherT String (MultiRWST r w s m) [HsFunctionDecl]
transformDecl tcs ds mn tDeclMap (TypeSig _loc names qtype)
  = insName qtype $ do
      (ctype, _) <- convertType tcs (Just mn) ds tDeclMap qtype
      return $ helper mn ctype <$> names
transformDecl _ _ _ _ _ = return []

transformDecl'
  :: (MonadMultiState ConvData m, Monad m, Functor m)
  => [HsTypeClass]
  -> [QualifiedName]
  -> ModuleName
  -> TypeDeclMap
  -> Decl
  -> EitherT String m [HsFunctionDecl]
transformDecl' tcs ds mn tDeclMap (TypeSig _loc names qtype)
  = insName qtype $ do
      ctype <- convertTypeInternal tcs (Just mn) ds tDeclMap qtype
      return $ helper mn ctype <$> names
transformDecl' _ _ _ _ _ = return []

insName :: (Functor m, Monad m)
        => Type -> EitherT String m a -> EitherT String m a
insName qtype = bimapEitherT (\x -> x ++ " in " ++ prettyPrint qtype) id

helper :: ModuleName -> HsType -> Name -> HsFunctionDecl
helper mn t x = (convertModuleName mn x, forallify t)

getDataConss
  :: (Monad m)
  => [HsTypeClass]
  -> [QualifiedName]
  -> TypeDeclMap
  -> [Module]
  -> MultiRWST
       r
       w
       s
       m
       [Either String ([HsFunctionDecl], DeconstructorBinding)]
getDataConss tcs ds tDeclMap modules = sequence $ do
  Module _loc moduleName _pragma _warning _mexp _imp decls <- modules
  DataDecl _loc _newtflag cntxt name params conss _derives <- decls
  let
    rTypeM :: ( MonadMultiState ConvData m )
           => EitherT String m HsType
    rTypeM = do
      let rName = convertModuleName moduleName name
      ps  <- mapM pTransform params
      return $ (forallify . foldl TypeApp (TypeCons rName)) ps
    pTransform :: MonadMultiState ConvData m => TyVarBind -> EitherT String m HsType
    pTransform (KindedVar _ _) = left "KindedVar"
    pTransform (UnkindedVar n) = TypeVar <$> getVar n
  --let
  --  tTransform (UnBangedTy t) = convertTypeInternal t
  --  tTransform x              = lift $ left $ "unknown Type: " ++ show x
  let
    typeM :: ( MonadMultiState ConvData m )
          => QualConDecl
          -> EitherT String m (QualifiedName, [HsType])
    typeM (QualConDecl _loc cbindings ccntxt conDecl) = do
      case cntxt of
        [] -> right ()
        _  -> left "context in data type"
      case (cbindings, ccntxt) of
        ([],[]) -> right ()
        _       -> left "constraint or existential type for constructor"
      (cname,tys) <- case conDecl of
        ConDecl c t -> right (c, t)
        x           -> left $ "unknown ConDecl: " ++ show x
      convTs <- convertTypeInternal tcs (Just moduleName) ds tDeclMap `mapM` tys
      let qName = convertModuleName moduleName cname
      return $ (qName, convTs)
  let
    addConsMsg = (++) $ show name ++ ": "
  let
    convAction :: ( MonadMultiState ConvData m )
               => EitherT String m ([HsFunctionDecl], DeconstructorBinding)
    convAction = do
      rtype  <- rTypeM
      consDatas <- mapM typeM conss
      return $ ( [ (n, foldr TypeArrow rtype ts)
                 | (n, ts) <- consDatas
                 ]
               , (rtype, consDatas, False)
               )
        -- TODO: actually determine if stuff is recursive or not
  return $ do
    convResult <- withMultiStateA (ConvData 0 M.empty) $ runEitherT convAction
    return $ either (Left . addConsMsg) Right convResult
    -- TODO: replace this by bimap..

getClassMethods
  :: (Monad m, Functor m)
  => [HsTypeClass]
  -> [QualifiedName]
  -> TypeDeclMap
  -> [Module]
  -> MultiRWST r w s m [Either String HsFunctionDecl]
getClassMethods tcs ds tDeclMap modules = fmap join $ sequence $ do
  Module _loc moduleName _pragma _warning _mexp _imp decls <- modules
  ClassDecl _ _ name@(Ident nameStr) vars _ cdecls <- decls
  return $ do
    let errorMod = (++) ("class method for "++show name++": ")
    let tcsTuples = (\tc -> (tclass_name tc, tc)) <$> tcs
    let searchF (QualifiedName _ n) = n==nameStr
        searchF _                   = False
    let maybeClass = snd <$> find (searchF . fst) tcsTuples
    case maybeClass of
      Nothing -> return [Left $ "unknown type class: "++show name]
      Just cls -> do
        let cnstrA = HsConstraint cls <$> mapM ((TypeVar <$>) . tyVarTransform) vars
        --     action :: ( MonadMultiState ConvData m ) => m [Either String [HsFunctionDecl]]
        rEithers <- withMultiStateA (ConvData 0 M.empty) $ do
          cnstrE <- runEitherT cnstrA
          case cnstrE of
            Left x -> return [Left x]
            Right cnstr ->
              mapM ( runEitherT
                   . fmap (map (addConstraint cnstr))
                   . transformDecl' tcs ds moduleName tDeclMap)
                $ [ d | ClsDecl d <- cdecls ]
        let _ = rEithers :: [Either String [HsFunctionDecl]]
        return $ concatMap (either (return . Left . errorMod) (map Right))
               $ rEithers
  where
    addConstraint :: HsConstraint -> HsFunctionDecl -> HsFunctionDecl
    addConstraint c (n, TypeForall vs cs t) = (n, TypeForall vs (c:cs) t)
    addConstraint _ _                       = error "addConstraint for non-forall type = bad"
      --(n, ForallType [] [c] t)

getDataTypes :: [Module] -> [QualifiedName]
getDataTypes modules = d1 ++ d2
 where
  d1 = do
    Module _loc moduleName _pragma _warning _mexp _imp decls <- modules
    DataDecl _ _ _ name _ _ _ <- decls
    return $ convertModuleName moduleName name
  d2 = do
    Module _loc moduleName _pragma _warning _mexp _imp decls <- modules
    TypeDecl _ name _ _ <- decls
    return $ convertModuleName moduleName name

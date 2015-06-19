{-# LANGUAGE TupleSections #-}

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

import Debug.Trace



getDecls :: [QualifiedName]
         -> [HsTypeClass]
         -> [Module]
         -> [Either String HsFunctionDecl]
getDecls ds tcs modules = do
  Module _loc mn _pragma _warning _mexp _imp decls <- modules
  concatMap
    (either (return.Left) (map Right) . transformDecl tcs ds mn)
    decls

transformDecl :: [HsTypeClass]
              -> [QualifiedName]
              -> ModuleName
              -> Decl
              -> Either String [HsFunctionDecl]
transformDecl tcs ds mn (TypeSig _loc names qtype)
  = insName qtype
  $ ((<$> names) . helper mn) <$> convertType tcs (Just mn) ds qtype
transformDecl _ _ _ _ = return []

transformDecl' :: [HsTypeClass]
               -> [QualifiedName]
               -> ModuleName
               -> Decl
               -> ConversionMonad [HsFunctionDecl]
transformDecl' tcs ds mn (TypeSig _loc names qtype)
  = mapEitherT (insName qtype <$>)
  $ (<$> names) . helper mn <$> convertTypeInternal tcs (Just mn) ds qtype
transformDecl' _ _ _ _ = return []

insName :: Type -> Either String a -> Either String a
insName qtype = either (\x -> Left $ x ++ " in " ++ prettyPrint qtype) Right

helper :: ModuleName -> HsType -> Name -> HsFunctionDecl
helper mn t x = (convertModuleName mn x, forallify t)

-- type ConversionMonad = EitherT String (State (Int, ConvMap))
getDataConss :: [HsTypeClass]
             -> [QualifiedName]
             -> [Module]
             -> [Either String ( [HsFunctionDecl]
                               , DeconstructorBinding )]
getDataConss tcs ds modules = do
  Module _loc moduleName _pragma _warning _mexp _imp decls <- modules
  DataDecl _loc _newtflag cntxt name params conss _derives <- decls
  let
    rTypeM :: ConversionMonad HsType
    rTypeM = fmap ( forallify
                  . foldl TypeApp (TypeCons $ convertModuleName moduleName name))
           $ mapM pTransform params
    pTransform :: TyVarBind -> ConversionMonad HsType
    pTransform (KindedVar _ _) = left $ "KindedVar"
    pTransform (UnkindedVar n) = TypeVar <$> getVar n
  --let
  --  tTransform (UnBangedTy t) = convertTypeInternal t
  --  tTransform x              = lift $ left $ "unknown Type: " ++ show x
  let
    typeM :: QualConDecl -> ConversionMonad (QualifiedName, [HsType])
    typeM (QualConDecl _loc cbindings ccntxt conDecl) = do
      case cntxt of
        [] -> right ()
        _  -> left $ "context in data type"
      case (cbindings, ccntxt) of
        ([],[]) -> right ()
        _       -> left $ "constraint or existential type for constructor"
      (cname,tys) <- case conDecl of
        ConDecl c t -> right (c, t)
        x           -> left $ "unknown ConDecl: " ++ show x
      convTs <- convertTypeInternal tcs (Just moduleName) ds `mapM` tys
      let qname = convertModuleName moduleName cname
      return $ (qname, convTs)
  let
    addConsMsg = (++) $ show name ++ ": "
  let
    convAction :: ConversionMonad ([HsFunctionDecl], DeconstructorBinding)
    convAction = do
      rtype  <- rTypeM
      consDatas <- mapM typeM conss
      return $ ( [ (n, foldr TypeArrow rtype ts)
                 | (n, ts) <- consDatas
                 ]
               , (rtype, consDatas, False)
               )
        -- TODO: actually determine if stuff is recursive or not
  return $ either (Left . addConsMsg) Right
         $ evalState (runEitherT $ convAction) (0, M.empty)
    -- TODO: replace this by bimap..

getClassMethods :: [HsTypeClass]
                -> [QualifiedName]
                -> [Module]
                -> [Either String HsFunctionDecl]
getClassMethods tcs ds modules = do
  Module _loc moduleName _pragma _warning _mexp _imp decls <- modules
  ClassDecl _ _ name@(Ident nameStr) vars _ cdecls <- decls
  let errorMod = (++) ("class method for "++show name++": ")
  let instClass = find (\(HsTypeClass (QualifiedName _ n) _ _)
                        -> n==nameStr) tcs
  case instClass of
    Nothing -> return $ Left $ "unknown type class: "++show name
    Just cls -> let
      cnstrA = HsConstraint cls <$> mapM ((TypeVar <$>) . tyVarTransform) vars
      action :: StateT (Int, ConvMap) Identity [Either String [HsFunctionDecl]]
      action = do
        cnstrE <- runEitherT cnstrA
        case cnstrE of
          Left x -> return [Left x]
          Right cnstr ->
            mapM ( runEitherT
                 . fmap (map (addConstraint cnstr))
                 . transformDecl' tcs ds moduleName)
              $ [ d | ClsDecl d <- cdecls ]
      in concatMap (either (return . Left . errorMod)
                           (map Right))
                 $ runIdentity
                 $ evalStateT action (0, M.empty)
  where
    addConstraint :: HsConstraint -> HsFunctionDecl -> HsFunctionDecl
    addConstraint c (n, TypeForall vs cs t) = (n, TypeForall vs (c:cs) t)
    addConstraint _ _                       = error "addConstraint for non-forall type = bad"
      --(n, ForallType [] [c] t)

getDataTypes :: [Module] -> [QualifiedName]
getDataTypes modules = do
  Module _loc moduleName _pragma _warning _mexp _imp decls <- modules
  DataDecl _ _ _ name _ _ _ <- decls
  return $ convertModuleName moduleName name

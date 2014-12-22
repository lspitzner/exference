{-# LANGUAGE TupleSections #-}

module Language.Haskell.Exference.BindingsFromHaskellSrc
  ( getDecls
  , declToBinding
  , getDataConss
  , getClassMethods
  )
where



import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exference.FunctionBinding
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.ConstrainedType
import Language.Haskell.Exference.Type
import Language.Haskell.Exference.TypeClasses
import Language.Haskell.Exference.FunctionDecl

import Control.Applicative ( (<$>), (<*>) )

import Control.Monad ( join )
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.List ( find )



getDecls :: StaticClassEnv -> Module -> [Either String HsFunctionDecl]
getDecls env (Module _loc _m _pragma _warning _mexp _imp decls)
  = concatMap (either (return.Left) (map Right) . transformDecl env) decls

transformDecl :: StaticClassEnv -> Decl -> Either String [HsFunctionDecl]
transformDecl env (TypeSig _loc names qtype)
  = insName qtype
  $ ((<$> names) . helper) <$> convertCType env qtype
transformDecl _ _ = return []

transformDecl' :: StaticClassEnv
              -> Decl
              -> ConversionMonad [HsFunctionDecl]
transformDecl' env (TypeSig _loc names qtype)
  = mapEitherT (insName qtype <$>)
  $ (<$> names) . helper <$> convertCTypeInternal env qtype
transformDecl' _ _ = return []

insName :: Type -> Either String a -> Either String a
insName qtype = either (\x -> Left $ x ++ " in " ++ prettyPrint qtype) Right

helper :: HsConstrainedType -> Name -> HsFunctionDecl
helper ct x = (hsNameToString x, ct)

-- type ConversionMonad = EitherT String (State (Int, ConvMap))
getDataConss :: Module -> [Either String ( [HsFunctionDecl]
                                         , DeconstructorBinding )]
getDataConss (Module _loc _m _pragma _warning _mexp _imp decls) = do
  DataDecl _loc _newtflag cntxt name params conss _derives <- decls
  let
    rTypeM :: ConversionMonad HsType
    rTypeM = fmap (foldl TypeApp (TypeCons $ hsNameToString name))
           $ mapM pTransform params
    pTransform :: TyVarBind -> ConversionMonad HsType
    pTransform (KindedVar _ _) = left $ "KindedVar"
    pTransform (UnkindedVar n) = TypeVar <$> getVar n
  --let
  --  tTransform (UnBangedTy t) = convertTypeInternal t
  --  tTransform x              = lift $ left $ "unknown Type: " ++ show x
  let
    typeM :: QualConDecl -> ConversionMonad (String, [HsType])
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
      convTs <- mapM convertTypeInternal tys
      let nameStr = hsNameToString cname
      return $ (nameStr, convTs)
  let
    addConsMsg = (++) $ hsNameToString name ++ ": "
  let
    convAction :: ConversionMonad ([HsFunctionDecl], DeconstructorBinding)
    convAction = do
      rtype  <- rTypeM
      consDatas <- mapM typeM conss
      return $ ( [ (n, HsConstrainedType [] $ foldr TypeArrow rtype ts)
                 | (n, ts) <- consDatas
                 ]
               , (rtype, consDatas, False)
               )
        -- TODO: actually determine if stuff is recursive or not
  return $ either (Left . addConsMsg) Right
         $ evalState (runEitherT $ convAction) (0, M.empty)
    -- TODO: replace this by bimap..

getClassMethods :: StaticClassEnv -> Module -> [Either String HsFunctionDecl]
getClassMethods env (Module _loc _m _pragma _warning _mexp _imp decls) =
  do
    ClassDecl _ _ name vars _ cdecls <- decls
    let nameStr = hsNameToString name
    let errorMod = (++) ("class method for "++nameStr++": ")
    let instClass = find ((nameStr==).tclass_name)
                  $ sClassEnv_tclasses env
    case instClass of
      Nothing -> return $ Left $ "unknown type class: "++nameStr
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
                   . transformDecl' env)
                $ [ d | ClsDecl d <- cdecls ]
        in concatMap (either (return . Left . errorMod)
                             (map Right))
                   $ runIdentity
                   $ evalStateT action (0, M.empty)
  where
    addConstraint :: HsConstraint -> HsFunctionDecl -> HsFunctionDecl
    addConstraint c (n, HsConstrainedType constrs t) =
                    (n, HsConstrainedType (c:constrs) t)

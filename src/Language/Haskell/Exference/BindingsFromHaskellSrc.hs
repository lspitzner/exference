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
import Language.Haskell.Exference.Types
import Language.Haskell.Exference.TypeUtils
import Language.Haskell.Exference.FunctionDecl

import Control.Applicative ( (<$>), (<*>) )

import Control.Monad ( join )
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.List ( find )



getDecls :: [HsTypeClass] -> Module -> [Either String HsFunctionDecl]
getDecls tcs (Module _loc _m _pragma _warning _mexp _imp decls)
  = concatMap (either (return.Left) (map Right) . transformDecl tcs) decls

transformDecl :: [HsTypeClass] -> Decl -> Either String [HsFunctionDecl]
transformDecl tcs (TypeSig _loc names qtype)
  = insName qtype
  $ ((<$> names) . helper) <$> convertType tcs qtype
transformDecl _ _ = return []

transformDecl' :: [HsTypeClass]
              -> Decl
              -> ConversionMonad [HsFunctionDecl]
transformDecl' tcs (TypeSig _loc names qtype)
  = mapEitherT (insName qtype <$>)
  $ (<$> names) . helper <$> convertTypeInternal tcs qtype
transformDecl' _ _ = return []

insName :: Type -> Either String a -> Either String a
insName qtype = either (\x -> Left $ x ++ " in " ++ prettyPrint qtype) Right

helper :: HsType -> Name -> HsFunctionDecl
helper t x = (hsNameToString x, forallify t)

-- type ConversionMonad = EitherT String (State (Int, ConvMap))
getDataConss :: [HsTypeClass] -> Module -> [Either String ( [HsFunctionDecl]
                                         , DeconstructorBinding )]
getDataConss tcs (Module _loc _m _pragma _warning _mexp _imp decls) = do
  DataDecl _loc _newtflag cntxt name params conss _derives <- decls
  let
    rTypeM :: ConversionMonad HsType
    rTypeM = fmap ( forallify
                  . foldl TypeApp (TypeCons $ hsNameToString name))
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
      convTs <- convertTypeInternal tcs `mapM` tys
      let nameStr = hsNameToString cname
      return $ (nameStr, convTs)
  let
    addConsMsg = (++) $ hsNameToString name ++ ": "
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

getClassMethods :: [HsTypeClass] -> Module -> [Either String HsFunctionDecl]
getClassMethods tcs (Module _loc _m _pragma _warning _mexp _imp decls) =
  do
    ClassDecl _ _ name vars _ cdecls <- decls
    let nameStr = hsNameToString name
    let errorMod = (++) ("class method for "++nameStr++": ")
    let instClass = find ((nameStr==).tclass_name) tcs
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
                   . transformDecl' tcs)
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

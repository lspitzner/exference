module Language.Haskell.Exference.BindingsFromHaskellSrc
  ( getBindings
  , getDataConss
  )
where



import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exference.FunctionBinding
import Language.Haskell.Exference.TypeFromHaskellSrc
import Language.Haskell.Exference.ConstrainedType
import Language.Haskell.Exference.Type
import Language.Haskell.Exference.TypeClasses

import Control.Applicative ( (<$>), (<*>) )

import Control.Monad ( join )
import Control.Monad.Identity
import Control.Monad.Trans.Either
import Control.Monad.State.Strict
import qualified Data.Map as M



getBindings :: StaticContext -> Module -> [Either String FunctionBinding]
getBindings context (Module _loc _m _pragma _warning _mexp _imp decls)
  = concatMap (either (return.Left) (map Right) . getFromDecls) decls
  where
    getFromDecls :: Decl -> Either String [FunctionBinding]
    getFromDecls (TypeSig _loc names qtype)
      = either (\x -> Left $ x ++ " in " ++ prettyPrint qtype) Right
      $ mapM ((<$> convertCType context qtype) . helper) names
    getFromDecls _ = return []

helper :: Name -> HsConstrainedType -> FunctionBinding
helper (Ident  n) ct = (n, ct)
helper (Symbol n) ct = (n, ct)

-- type ConversionMonad = StateT (Int, ConvMap) (EitherT String Identity)
getDataConss :: Module -> [Either String FunctionBinding]
getDataConss (Module _loc _m _pragma _warning _mexp _imp decls) = do
  DataDecl _loc _newtflag cntxt name params conss _derives <- decls
  let
    rTypeM :: ConversionMonad HsType
    rTypeM = fmap (foldl TypeApp (TypeCons $ hsNameToString name))
           $ mapM pTransform params
    pTransform :: TyVarBind -> ConversionMonad HsType
    pTransform (KindedVar _ _) = lift $ left $ "KindedVar"
    pTransform (UnkindedVar n) = TypeVar <$> getVar n
  --let
  --  tTransform (UnBangedTy t) = convertTypeInternal t
  --  tTransform x              = lift $ left $ "unknown Type: " ++ show x
  let
    typeM :: QualConDecl -> ConversionMonad FunctionBinding
    typeM (QualConDecl _loc cbindings ccntxt conDecl) =
      case (cntxt, cbindings, ccntxt, conDecl) of
        ([], [], [], ConDecl cname tys) -> do
          convTs <- mapM convertTypeInternal tys
          rtype  <- rTypeM
          return $ helper cname $ HsConstrainedType
            []
            (foldr TypeArrow rtype convTs)
        ([], [], [], x) ->
          lift $ left $ "unknown ConDecl: " ++ show x
        ([], _, _, _) ->
          lift $ left $ "constraint or existential type for constructor"
        _ ->
          lift $ left $ "context in data type"
  either (return.Left) (map Right)
    $ mapM (\x -> runIdentity $ runEitherT $ evalStateT (typeM x) (0, M.empty))
    $ conss


{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Haskell.Exference.TypeFromHaskellSrc
  ( convertType
  , convertTypeInternal
  , convertName
  , convertQName
  , convertModuleName
  , ConvMap
  , getVar
  , ConversionMonad
  , parseType
  , parseQualifiedName
  , unsafeReadType
  , unsafeReadType0
  , tyVarTransform
  , haskellSrcExtsParseMode
  , findInvalidNames
  )
where



import Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.Parser as P

import qualified Language.Haskell.Exference.Types as T
import qualified Language.Haskell.Exference.TypeUtils as TU
import qualified Data.Map as M

import Control.Applicative ( (<$>), (<*>) )
import Data.Maybe ( fromMaybe )
import Data.List ( find )

import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Control.Monad.Trans.Either

import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )

import Debug.Trace



type ConversionMonad = EitherT String (State (Int, ConvMap))

haskellSrcExtsParseMode :: String -> P.ParseMode
haskellSrcExtsParseMode s = P.ParseMode (s++".hs")
                                      Haskell2010
                                      exts2
                                      False
                                      False
                                      Nothing
  where
    exts1 = [ TypeOperators
            , ExplicitForAll
            , ExistentialQuantification
            , TypeFamilies
            , FunctionalDependencies
            , FlexibleContexts
            , MultiParamTypeClasses ]
    exts2 = map EnableExtension exts1

convertType :: [T.HsTypeClass]
            -> Maybe ModuleName
            -> [T.QualifiedName]
            -> Type
            -> Either String T.HsType
convertType tcs mn ds t =
  evalState (runEitherT $ convertTypeInternal tcs mn ds t) (0, M.empty)

convertTypeInternal :: [T.HsTypeClass]
                    -> Maybe ModuleName -- default (for unqualified stuff)
                      -- Nothing uses a broad search for lookups
                    -> [T.QualifiedName] -- list of fully qualified data types
                                         -- (to keep things unique)
                    -> Type
                    -> ConversionMonad T.HsType
convertTypeInternal tcs defModuleName ds ty = helper ty
 where
  helper (TyFun a b)      = T.TypeArrow
                              <$> helper a
                              <*> helper b
  helper (TyTuple _ ts)   | n <- length ts
                          = foldl
                              T.TypeApp
                              (T.TypeCons (T.TupleCon n))
                            <$> mapM helper ts
  helper (TyApp a b)      = T.TypeApp
                              <$> helper a
                              <*> helper b
  helper (TyVar vname)    = do
                              i <- getVar vname
                              return $ T.TypeVar i
  helper (TyCon name)     = return
                              $ T.TypeCons
                              $ convertQName defModuleName ds name
  helper (TyList t)       = T.TypeApp (T.TypeCons T.ListCon)
                              <$> helper t
  helper (TyParen t)      = helper t
  helper (TyInfix _ _ _)  = left "infix operator"
  helper (TyKind _ _)     = left "kind annotation"
  helper (TyPromoted _)   = left "promoted type"
  helper (TyForall maybeTVars cs t) =
    T.TypeForall
      <$> case maybeTVars of
            Nothing -> return []
            Just tvs -> tyVarTransform `mapM` tvs
      <*> convertConstraint tcs defModuleName ds `mapM` cs
      <*> helper t
  helper x                = left $ "unknown type element: " ++ show x -- TODO

type ConvMap = M.Map Name Int

getVar :: MonadState (Int, ConvMap) m => Name -> m Int
getVar n = do
  (next, m) <- get
  case M.lookup n m of
    Nothing -> do
      put (next+1, M.insert n next m)
      return next
    Just i ->
      return i

-- defaultModule -> potentially-qualified-name-thingy -> exference-q-name
convertQName :: Maybe ModuleName -> [T.QualifiedName] -> QName -> T.QualifiedName
convertQName _ _ (Special UnitCon)          = T.TupleCon 0
convertQName _ _ (Special ListCon)          = T.ListCon
convertQName _ _ (Special FunCon)           = error "no support for FunCon" -- i wonder how we reach this..
convertQName _ _ (Special (TupleCon _ i))   = T.TupleCon i
convertQName _ _ (Special Cons)             = T.Cons
convertQName _ _ (Special UnboxedSingleCon) = T.TupleCon 0
convertQName _ _ (Qual mn s)                = convertModuleName mn s
convertQName (Just d) _ (UnQual s)          = convertModuleName d s
convertQName Nothing ds (UnQual (Ident s))  = fromMaybe (T.QualifiedName [] s)
                                              $ find p ds
 where
  p (T.QualifiedName _ x) = x==s
  p _ = False
convertQName Nothing _ (UnQual s)           = convertName s

convertName :: Name -> T.QualifiedName
convertName (Ident s)  = T.QualifiedName [] s
convertName (Symbol s) = T.QualifiedName [] $ "(" ++ s ++ ")"

convertModuleName :: ModuleName -> Name -> T.QualifiedName
convertModuleName (ModuleName n) (Ident s)  = parseQualifiedName
                                            $ n ++ "." ++ s
convertModuleName (ModuleName n) (Symbol s) = parseQualifiedName
                                            $ n ++ ".(" ++ s ++ ")"

parseQualifiedName :: String -> T.QualifiedName
parseQualifiedName s = helper s []
 where
  helper :: String -> [String] -> T.QualifiedName
  helper n ns = case span (/='.') n of
    (final, []) -> T.QualifiedName (reverse ns) final
    (part, (_:rest)) -> helper rest (part:ns)

convertConstraint :: [T.HsTypeClass]
                  -> Maybe ModuleName
                  -> [T.QualifiedName]
                  -> Asst
                  -> ConversionMonad T.HsConstraint
convertConstraint tcs defModuleName@(Just _) ds (ClassA qname types)
  | str    <- convertQName defModuleName ds qname
  , ctypes <- mapM (convertTypeInternal tcs defModuleName ds) types
  = T.HsConstraint ( fromMaybe TU.unknownTypeClass
                   $ find ((==str).T.tclass_name)
                   $ tcs) <$> ctypes
convertConstraint tcs Nothing ds (ClassA (UnQual (Symbol "[]")) types)
  | ctypes <- mapM (convertTypeInternal tcs Nothing ds) types
  = T.HsConstraint ( fromMaybe TU.unknownTypeClass
                   $ find (\(T.HsTypeClass x _ _)
                           -> case x of { T.ListCon -> True; _ -> False })
                   $ tcs) <$> ctypes
convertConstraint tcs Nothing ds (ClassA (UnQual (Ident name)) types)
  | ctypes <- mapM (convertTypeInternal tcs Nothing ds) types
  = T.HsConstraint ( fromMaybe TU.unknownTypeClass
                   $ find (\(T.HsTypeClass (T.QualifiedName _ n) _ _)
                           -> n==name)
                   $ tcs) <$> ctypes
convertConstraint tcs _ ds (ClassA q@(Qual {}) types)
  | ctypes <- mapM (convertTypeInternal tcs Nothing ds) types
  , name <- convertQName Nothing ds q
  = T.HsConstraint ( fromMaybe TU.unknownTypeClass
                   $ find (\(T.HsTypeClass n _ _)
                           -> n==name)
                   $ tcs) <$> ctypes
convertConstraint _ Nothing _ cls@ClassA{} = error $ "convertConstraint" ++ show cls
convertConstraint env defModuleName ds (ParenA c)
  = convertConstraint env defModuleName ds c
convertConstraint _ _ _ c
  = left $ "bad constraint: " ++ show c

parseType :: [T.HsTypeClass]
          -> Maybe ModuleName
          -> [T.QualifiedName]
          -> P.ParseMode
          -> String
          -> Either String T.HsType
parseType tcs mn ds m s = case P.parseTypeWithMode m s of
  f@(P.ParseFailed _ _) -> Left $ show f
  P.ParseOk t -> convertType tcs mn ds t

unsafeReadType :: [T.HsTypeClass] -> [T.QualifiedName] -> String -> T.HsType
unsafeReadType tcs ds s
  = case parseType tcs Nothing ds (haskellSrcExtsParseMode "type") s of
      Left _ -> error $ "unsafeReadType: could not parse type: " ++ s
      Right t -> t

unsafeReadType0 :: String -> T.HsType
unsafeReadType0 s
  = case parseType [] Nothing [] (haskellSrcExtsParseMode "type") s of
      Left _ -> error $ "unsafeReadType: could not parse type: " ++ s
      Right t -> t

tyVarTransform :: TyVarBind
               -> ConversionMonad T.TVarId
tyVarTransform (KindedVar _ _) = left $ "KindedVar"
tyVarTransform (UnkindedVar n) = getVar n

findInvalidNames :: [T.QualifiedName] -> T.HsType -> [T.QualifiedName]
findInvalidNames _ T.TypeVar {}          = []
findInvalidNames _ T.TypeConstant {}     = []
findInvalidNames valids (T.TypeCons n)
                 | (T.QualifiedName _ _) <- n
                 = [ n | n `notElem` valids ]                   
                 | otherwise = []
findInvalidNames valids (T.TypeArrow t1 t2)   =
  findInvalidNames valids t1 ++ findInvalidNames valids t2
findInvalidNames valids (T.TypeApp t1 t2)     =
  findInvalidNames valids t1 ++ findInvalidNames valids t2
findInvalidNames valids (T.TypeForall _ _ t1) =
  findInvalidNames valids t1


{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}

module Language.Haskell.Exference.TypeFromHaskellSrc
  ( ConvData(..)
  , convertTypeNoDecl
  , convertTypeNoDeclInternal
  , convertName
  , convertQName
  , convertModuleName
  , getVar
  -- , ConversionMonad
  , parseQualifiedName
  , tyVarTransform
  , haskellSrcExtsParseMode
  , findInvalidNames
  )
where



import Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.Parser as P

import qualified Language.Haskell.Exference.Core.Types as T
import qualified Language.Haskell.Exference.Core.TypeUtils as TU
import qualified Data.Map as M

import Control.Applicative ( (<$>), (<*>), Applicative )
import Data.Maybe ( fromMaybe )
import Data.List ( find )

import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Control.Monad.Trans.Either

import Control.Monad.Trans.MultiRWS
import Control.Monad.Trans.MultiState ( MonadMultiState(..) )
import Data.HList.ContainsType

import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )

import Debug.Trace



-- type ConversionMonad = EitherT String (State (Int, ConvMap))

data ConvData = ConvData Int T.TypeVarIndex

haskellSrcExtsParseMode :: String -> P.ParseMode
haskellSrcExtsParseMode s = P.ParseMode (s++".hs")
                                      Haskell2010
                                      exts2
                                      False
                                      False
                                      Nothing
                                      False
  where
    exts1 = [ TypeOperators
            , ExplicitForAll
            , ExistentialQuantification
            , TypeFamilies
            , FunctionalDependencies
            , FlexibleContexts
            , MultiParamTypeClasses ]
    exts2 = map EnableExtension exts1

convertTypeNoDecl :: ( ContainsType T.QNameIndex s
               , Monad m
               )
            => [T.HsTypeClass]
            -> Maybe ModuleName
            -> [T.QualifiedName]
            -> Type
            -> EitherT String (MultiRWST r w s m) (T.HsType, T.TypeVarIndex)
convertTypeNoDecl tcs mn ds t =
  mapEitherT conv $ convertTypeNoDeclInternal tcs mn ds t
 where
  conv m = [ [ (r, index)
             | r <- eith
             ]
           | (eith, ConvData _ index) <- withMultiStateAS (ConvData 0 M.empty) m
           ]

convertTypeNoDeclInternal :: ( MonadMultiState T.QNameIndex m
                       , MonadMultiState ConvData m
                       )
                    => [T.HsTypeClass]
                    -> Maybe ModuleName -- default (for unqualified stuff)
                      -- Nothing uses a broad search for lookups
                    -> [T.QualifiedName] -- list of fully qualified data types
                                         -- (to keep things unique)
                    -> Type
                    -> EitherT String m T.HsType
convertTypeNoDeclInternal tcs defModuleName ds ty = helper ty
 where
  helper (TyFun a b)      = T.TypeArrow
                              <$> helper a
                              <*> helper b
  helper (TyTuple _ ts)   | n <- length ts
                          = foldl T.TypeApp . T.TypeCons
                            <$> TU.getOrCreateQNameId (T.TupleCon n)
                            <*> mapM helper ts
  helper (TyApp a b)      = T.TypeApp
                              <$> helper a
                              <*> helper b
  helper (TyVar vname)    = do
                              i <- getVar vname
                              return $ T.TypeVar i
  helper (TyCon name)     = T.TypeCons
                          <$> TU.getOrCreateQNameId
                              (convertQName defModuleName ds name)
  helper (TyList t)       = T.TypeApp . T.TypeCons
                          <$> TU.getOrCreateQNameId T.ListCon
                          <*> helper t
  helper (TyParen t)      = helper t
  helper TyInfix{}        = left "infix operator"
  helper TyKind{}         = left "kind annotation"
  helper TyPromoted{}     = left "promoted type"
  helper (TyForall maybeTVars cs t) =
    T.TypeForall
      <$> case maybeTVars of
            Nothing -> return []
            Just tvs -> tyVarTransform `mapM` tvs
      <*> convertConstraint tcs defModuleName ds `mapM` cs
      <*> helper t
  helper x                = left $ "unknown type element: " ++ show x -- TODO

getVar :: MonadMultiState ConvData m => Name -> m Int
getVar n = do
  ConvData next m <- mGet
  case M.lookup n m of
    Nothing -> do
      mSet $ ConvData (next+1) (M.insert n next m)
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
                                            $ "(" ++ n ++ "." ++ s ++ ")"

parseQualifiedName :: String -> T.QualifiedName
parseQualifiedName s = case s of
  ""                                 -> T.QualifiedName [] ""
  [_]                                -> helper s                 [] False
  _ | head s == '(' && last s == ')' -> helper (tail $ init $ s) [] True
    | otherwise                      -> helper s                 [] False
 where
  helper :: String -> [String] -> Bool -> T.QualifiedName
  helper n ns isOperator = case span (/='.') n of
    (final, []) -> T.QualifiedName (reverse ns) $ if isOperator
                     then "(" ++ final ++ ")"
                     else final
    (part, _:rest) -> helper rest (part:ns) isOperator

convertConstraint :: ( MonadMultiState T.QNameIndex m
                     , MonadMultiState ConvData m
                     )
                  => [T.HsTypeClass]
                  -> Maybe ModuleName
                  -> [T.QualifiedName]
                  -> Asst
                  -> EitherT String m T.HsConstraint
convertConstraint tcs defModuleName@(Just _) ds (ClassA qname types)
  | str    <- convertQName defModuleName ds qname
  , ctypes <- mapM (convertTypeNoDeclInternal tcs defModuleName ds) types
  = do
      strId <- TU.getOrCreateQNameId str
      unknown <- TU.unknownTypeClass
      ts <- ctypes
      return $ T.HsConstraint ( fromMaybe unknown
                              $ find ((==strId) . T.tclass_name)
                              $ tcs)
                              ts
convertConstraint tcs Nothing ds (ClassA (UnQual (Symbol "[]")) types)
  | ctypes <- mapM (convertTypeNoDeclInternal tcs Nothing ds) types
  = do
      ts <- ctypes
      unknown <- TU.unknownTypeClass
      listId <- TU.getOrCreateQNameId T.ListCon
      return $ T.HsConstraint
                 ( fromMaybe unknown
                 $ find ((==listId) . T.tclass_name)
                 $ tcs )
                 ts
convertConstraint tcs Nothing ds (ClassA (UnQual (Ident name)) types)
  | ctypes <- mapM (convertTypeNoDeclInternal tcs Nothing ds) types
  = do
      ts <- ctypes
      unknown <- TU.unknownTypeClass
      tcsTuples <- tcs `forM` \tc ->
        [ (qn, tc)
        | qn <- TU.lookupQNameId $ T.tclass_name tc
        ]
      let searchF (Just (T.QualifiedName _ n)) = n==name
          searchF _                            = False
      let tc = fromMaybe unknown $ snd <$> find (searchF . fst) tcsTuples
      return $ T.HsConstraint tc ts
convertConstraint tcs _ ds (ClassA q@(Qual {}) types)
  | ctypes <- mapM (convertTypeNoDeclInternal tcs Nothing ds) types
  , name <- convertQName Nothing ds q
  = do
      ts <- ctypes
      unknown <- TU.unknownTypeClass
      nameId <- TU.getOrCreateQNameId name
      return $ T.HsConstraint
                 ( fromMaybe unknown
                 $ find (\(T.HsTypeClass n _ _)
                         -> n==nameId) tcs
                 )
                 ts
convertConstraint _ Nothing _ cls@ClassA{} = error $ "convertConstraint" ++ show cls
convertConstraint env defModuleName ds (ParenA c)
  = convertConstraint env defModuleName ds c
convertConstraint _ _ _ c
  = left $ "bad constraint: " ++ show c

tyVarTransform :: MonadMultiState ConvData m
               => TyVarBind
               -> EitherT String m T.TVarId
tyVarTransform (KindedVar _ _) = left $ "KindedVar"
tyVarTransform (UnkindedVar n) = getVar n

findInvalidNames :: ( MonadMultiState T.QNameIndex m
                    , Applicative m
                    )
                 => [T.QualifiedName]
                 -> T.HsType
                 -> m [T.QualifiedName]
findInvalidNames _ T.TypeVar {}          = return []
findInvalidNames _ T.TypeConstant {}     = return []
findInvalidNames valids (T.TypeCons qid) = do
  (T.QNameIndex _ _ indexB) <- mGet
  case M.lookup qid indexB of
    Just n@(T.QualifiedName _ _) -> return [ n | n `notElem` valids ]
    _                            -> return []
findInvalidNames valids (T.TypeArrow t1 t2)   =
  (++) <$> findInvalidNames valids t1 <*> findInvalidNames valids t2
findInvalidNames valids (T.TypeApp t1 t2)     =
  (++) <$> findInvalidNames valids t1 <*> findInvalidNames valids t2
findInvalidNames valids (T.TypeForall _ _ t1) =
  findInvalidNames valids t1


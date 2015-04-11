{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Haskell.Exference.TypeFromHaskellSrc
  ( convertType
  , convertTypeInternal
  , hsNameToString
  , hsQNameToString
  , ConvMap
  , getVar
  , ConversionMonad
  , parseType
  , unsafeReadType
  , unsafeReadType0
  , tyVarTransform
  , haskellSrcExtsParseMode
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

convertType :: [T.HsTypeClass] -> Type -> Either String T.HsType
convertType tcs t =
  evalState (runEitherT $ convertTypeInternal tcs t) (0, M.empty)

convertTypeInternal :: [T.HsTypeClass]
                    -> Type
                    -> ConversionMonad T.HsType
convertTypeInternal tcs (TyFun a b) = T.TypeArrow
                                      <$> convertTypeInternal tcs a
                                      <*> convertTypeInternal tcs b
convertTypeInternal tcs (TyTuple _ ts) | n <- length ts
                                       , name <- "Tuple" ++ show n
                                       = foldl T.TypeApp (T.TypeCons name)
                                         <$> mapM (convertTypeInternal tcs) ts
convertTypeInternal tcs (TyApp a b) = T.TypeApp
                                      <$> convertTypeInternal tcs a
                                      <*> convertTypeInternal tcs b
convertTypeInternal _   (TyVar vname)    = do
                                           i <- getVar vname
                                           return $ T.TypeVar i
convertTypeInternal _   (TyCon name)     = return
                                           $ T.TypeCons
                                           $ hsQNameToString name
convertTypeInternal tcs (TyList t)       = T.TypeApp (T.TypeCons "List")
                                           <$> convertTypeInternal tcs t
convertTypeInternal tcs (TyParen t)      = convertTypeInternal tcs t
convertTypeInternal _   (TyInfix _ _ _)  = left "infix operator"
convertTypeInternal _   (TyKind _ _)     = left "kind annotation"
convertTypeInternal _   (TyPromoted _)   = left "promoted type"
convertTypeInternal tcs (TyForall maybeTVars cs t) =
  TU.forallify <$>
    (T.TypeForall
      <$> case maybeTVars of
            Nothing -> return []
            Just tvs -> tyVarTransform `mapM` tvs
      <*> convertConstraint tcs `mapM` cs
      <*> convertTypeInternal tcs t)
convertTypeInternal _   x                = left $ "unknown type element: " ++ show x -- TODO

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

hsQNameToString :: QName -> String
hsQNameToString (Special UnitCon) = "Unit"
hsQNameToString (Special ListCon) = "List"
hsQNameToString (Special FunCon)  = "Function" -- TODO: this is wrong (probably)
hsQNameToString (Special (TupleCon _ i)) = "Tuple" ++ show i
hsQNameToString (Special Cons)    = undefined
hsQNameToString (Special UnboxedSingleCon) = undefined
hsQNameToString (Qual _ s) = hsNameToString s -- yeah, ignore that full qualification
hsQNameToString (UnQual s) = hsNameToString s

hsNameToString :: Name -> String
hsNameToString (Ident s) = s
hsNameToString (Symbol s) = "(" ++ s ++ ")"

convertConstraint :: [T.HsTypeClass]
                  -> Asst
                  -> ConversionMonad T.HsConstraint
convertConstraint tcs (ClassA qname types)
  | str <- hsQNameToString qname
  , ctypes <- mapM (convertTypeInternal tcs) types
  = T.HsConstraint ( fromMaybe TU.unknownTypeClass
                   $ find ((==str).T.tclass_name)
                   $ tcs) <$> ctypes
convertConstraint env (ParenA c) = convertConstraint env c
convertConstraint _ c = left $ "bad constraint: " ++ show c

parseType :: [T.HsTypeClass] -> P.ParseMode -> String -> Either String T.HsType
parseType tcs m s = case P.parseTypeWithMode m s of
  f@(P.ParseFailed _ _) -> Left $ show f
  P.ParseOk t -> convertType tcs t

unsafeReadType :: [T.HsTypeClass] -> String -> T.HsType
unsafeReadType tcs s = case parseType tcs (haskellSrcExtsParseMode "type") s of
  Left _ -> error $ "unsafeReadType: could not parse type: " ++ s
  Right t -> t

unsafeReadType0 :: String -> T.HsType
unsafeReadType0 s = case parseType [] (haskellSrcExtsParseMode "type") s of
  Left _ -> error $ "unsafeReadType: could not parse type: " ++ s
  Right t -> t

tyVarTransform :: TyVarBind
               -> ConversionMonad T.TVarId
tyVarTransform (KindedVar _ _) = left $ "KindedVar"
tyVarTransform (UnkindedVar n) = getVar n

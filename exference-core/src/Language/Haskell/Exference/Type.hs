{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Exference.Type
  ( TVarId
  , HsType (..)
  , Subst
  , Substs
  , applySubst
  , applySubsts
  , reduceIds
  , incVarIds
  , largestId
  , distinctify
  , arrowDepth
  , freeVars
  , containsVar
  , typeParser
  , badReadVar
  , showVar
  , largestSubstsId
  , forallify -- unused atm
  )
where



import Data.Char ( ord, chr, isLower, isUpper )
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Applicative ( (<$>), (<*>), (*>), (<*) )
import Data.List ( intercalate )
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Char

import Data.Maybe ( maybeToList, fromMaybe )

import Control.DeepSeq.Generics
import GHC.Generics

import Debug.Hood.Observe
import Debug.Trace



type TVarId = Int

data HsType = TypeVar TVarId
            | TypeCons String
            | TypeArrow HsType HsType
            | TypeApp   HsType HsType
            | TypeForall [TVarId] HsType
  deriving (Ord, Eq, Generic)

instance NFData HsType where rnf = genericRnf

type Subst  = (TVarId, HsType)
type Substs = M.Map TVarId HsType

showVar :: TVarId -> String
showVar i = if i<26 then [chr (ord 'a' + i)] else "t"++show (i-26)

badReadVar :: String -> TVarId
badReadVar [c] = ord c - ord 'a'
badReadVar _ = error "badReadVar: that's why it is called badReadVar"

instance Show HsType where
  showsPrec _ (TypeVar i) = showString $ showVar i
  showsPrec _ (TypeCons s) = showString s
  showsPrec d (TypeArrow t1 t2) =
    showParen (d> -2) $ showsPrec (-1) t1 . showString " -> " . showsPrec (-1) t2
  showsPrec d (TypeApp t1 t2) =
    showParen (d> -1) $ showsPrec 0 t1 . showString " " . showsPrec 0 t2
  showsPrec d (TypeForall is t) = showParen (d>0) $
    showString ("forall "
               ++ intercalate ", " (showVar <$> is) ++ " . ") . showsPrec 0 t

instance Observable HsType where
  observer x = observeOpaque (show x) x

instance Read HsType where
  readsPrec _ = maybeToList . parseType

parseType :: String -> Maybe (HsType, String)
parseType s = either (const Nothing) Just
            $ runParser (    (,)
                         <$> typeParser
                         <*> many anyChar)
                        ()
                        ""
                        s

typeParser :: Parser HsType
typeParser = parseAll
  where
    parseAll :: Parser HsType
    parseAll = parseUn >>= parseBin
    parseUn :: Parser HsType -- TODO: forall
    parseUn = spaces *> (
            try (TypeCons <$> ((:) <$> satisfy isUpper <*> many alphaNum))
        <|> try ((TypeVar . (\x -> x - ord 'a') . ord) <$> satisfy isLower)
        <|>     (char '(' *> parseAll <* char ')')
      )
    parseBin :: HsType -> Parser HsType
    parseBin left =
        try (    try (TypeArrow left <$> (spaces *> string "->" *> parseAll))
             <|>     ((TypeApp   left <$> (space *> parseUn)) >>= parseBin)
             )
        <|>
        (spaces *> return left)


arrowDepth :: HsType -> Int
arrowDepth (TypeVar _) = 1
arrowDepth (TypeCons _) = 1
arrowDepth (TypeArrow _ t) = 1 + arrowDepth t
arrowDepth (TypeApp _ _) = 1
arrowDepth (TypeForall _ t) = arrowDepth t

freeVars :: HsType -> S.Set TVarId
freeVars (TypeVar i) = S.singleton i
freeVars (TypeCons _) = S.empty
freeVars (TypeArrow t1 t2) = S.union (freeVars t1) (freeVars t2)
freeVars (TypeApp t1 t2) = S.union (freeVars t1) (freeVars t2)
freeVars (TypeForall is t) = foldr S.delete (freeVars t) is

containsVar :: TVarId -> HsType -> Bool
containsVar i = S.member i . freeVars

-- binds everything in Foralls, so there are no free variables anymore.
forallify :: HsType -> HsType
forallify t =
  let frees = freeVars t
  in TypeForall (S.toList frees) t

reduceIds :: HsType -> HsType
reduceIds t = evalState (f t) (M.empty, 0)
  where
    f :: HsType -> State (M.Map TVarId TVarId, TVarId) HsType
    f (TypeVar i) = TypeVar <$> g i
    f c@(TypeCons _) = return c
    f (TypeArrow t1 t2) = TypeArrow  <$> f t1 <*> f t2
    f (TypeApp   t1 t2) = TypeApp    <$> f t1 <*> f t2
    f (TypeForall is t1) = TypeForall <$> mapM g is  <*> f t1
    g :: TVarId -> State (M.Map TVarId TVarId, TVarId) TVarId
    g i = do
      (mapping, next) <- get
      case M.lookup i mapping of
        Nothing -> do
          put (M.insert i next mapping, next+1)
          return next
        Just x -> return x

incVarIds :: (TVarId -> TVarId) -> HsType -> HsType
incVarIds f (TypeVar i) = TypeVar (f i)
incVarIds f (TypeArrow t1 t2) = TypeArrow (incVarIds f t1) (incVarIds f t2)
incVarIds f (TypeApp t1 t2) = TypeApp (incVarIds f t1) (incVarIds f t2)
incVarIds f (TypeForall is t) = TypeForall (f <$> is) (incVarIds f t)
incVarIds _ t = t

largestId :: HsType -> TVarId
largestId (TypeVar i)       = i
largestId (TypeCons _)      = -1
largestId (TypeArrow t1 t2) = largestId t1 `max` largestId t2
largestId (TypeApp t1 t2)   = largestId t1 `max` largestId t2
largestId (TypeForall _ t)  = largestId t

distinctify :: HsType -> HsType -> HsType
distinctify a b = let x = largestId a in incVarIds (+(x+1)) b

applySubst :: Subst -> HsType -> HsType
applySubst (i, t) v@(TypeVar j) = if i==j then t else v
applySubst _ c@(TypeCons _) = c
applySubst s (TypeArrow t1 t2) = TypeArrow (applySubst s t1) (applySubst s t2)
applySubst s (TypeApp t1 t2) = TypeApp (applySubst s t1) (applySubst s t2)
applySubst s@(i,_) f@(TypeForall js t) = if elem i js then f else TypeForall js (applySubst s t)

applySubsts :: Substs -> HsType -> HsType
applySubsts s v@(TypeVar i) = fromMaybe v $ M.lookup i s
applySubsts _ c@(TypeCons _) = c
applySubsts s (TypeArrow t1 t2) = TypeArrow (applySubsts s t1) (applySubsts s t2)
applySubsts s (TypeApp t1 t2) = TypeApp (applySubsts s t1) (applySubsts s t2)
applySubsts s (TypeForall js t) = TypeForall js $ applySubsts (foldr M.delete s js) t

largestSubstsId :: Substs -> TVarId
largestSubstsId = M.foldl' (\a b -> a `max` largestId b) 0

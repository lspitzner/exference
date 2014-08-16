module Type where



import Data.Char ( ord, chr )
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Trans.State.Strict
import Control.Applicative ( (<$>), (<*>), (*>), (<*) )

import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Char

import Data.Char ( isLower, isUpper )
import Data.Maybe ( maybeToList )

import Debug.Hood.Observe
import Debug.Trace



type TVarId = Int

data HsType = TypeVar TVarId
            | TypeCons String
            | TypeArrow HsType HsType
            | TypeApp   HsType HsType
            | TypeForall TVarId HsType
  deriving (Ord, Eq)

type Subst  = (TVarId, HsType)
type Substs = M.Map TVarId HsType

showVar :: TVarId -> String
showVar i = if i<26 then [chr (ord 'a' + i)] else "t"++show (i-26)

badReadVar :: String -> TVarId
badReadVar [c] = ord c - ord 'a'
badReadVar _ = undefined

instance Show HsType where
  showsPrec _ (TypeVar i) = showString $ showVar i
  showsPrec _ (TypeCons s) = showString s
  showsPrec d (TypeArrow t1 t2) =
    showParen (d> -2) $ showsPrec (-1) t1 . showString " -> " . showsPrec (-1) t2
  showsPrec d (TypeApp t1 t2) =
    showParen (d> -1) $ showsPrec 0 t1 . showString " " . showsPrec 0 t2
  showsPrec d (TypeForall i t) = showParen (d>0) $
    (showString $ "forall " ++ showVar i ++ " . ") . showsPrec 0 t

instance Observable HsType where
  observer x parent = observeOpaque (show x) x parent

instance Read HsType where
  readsPrec _ = maybeToList . parseType

parseType :: String -> Maybe (HsType, String)
parseType s = either (const Nothing) Just
            $ runParser (    (,)
                         <$> typeParser
                         <*> (many anyChar))
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
freeVars (TypeForall i t) = S.delete i $ freeVars t

containsVar :: TVarId -> HsType -> Bool
containsVar i = S.member i . freeVars

-- binds everything in Foralls, so there are no free variables anymore.
forallify :: HsType -> HsType
forallify t =
  let frees = freeVars t
  in foldr TypeForall t (S.toList frees)

reduceIds :: HsType -> HsType
reduceIds t = evalState (f t) (M.empty, 0)
  where
    f :: HsType -> State (M.Map TVarId TVarId, TVarId) HsType
    f (TypeVar i) = TypeVar <$> g i
    f c@(TypeCons _) = return c
    f (TypeArrow t1 t2) = TypeArrow <$> f t1 <*> f t2
    f (TypeApp t1 t2) = TypeApp <$> f t1 <*> f t2
    f (TypeForall i t1) = TypeForall <$> (g i) <*> f t1
    g :: TVarId -> State (M.Map TVarId TVarId, TVarId) TVarId
    g i = do
      (mapping, next) <- get
      case M.lookup i mapping of
        Nothing -> do
          put $ (M.insert i next mapping, next+1)
          return next
        Just x -> return x

incVarIds :: (TVarId -> TVarId) -> HsType -> HsType
incVarIds f (TypeVar i) = TypeVar (f i)
incVarIds f (TypeArrow t1 t2) = TypeArrow (incVarIds f t1) (incVarIds f t2)
incVarIds f (TypeApp t1 t2) = TypeApp (incVarIds f t1) (incVarIds f t2)
incVarIds f (TypeForall i t) = TypeForall (f i) (incVarIds f t)
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
applySubst s@(i,_) f@(TypeForall j t) = if i==j then f else TypeForall j (applySubst s t)

applySubsts :: Substs -> HsType -> HsType
applySubsts s v@(TypeVar i) = case M.lookup i s of
  Nothing -> v
  Just t -> t
applySubsts _ c@(TypeCons _) = c
applySubsts s (TypeArrow t1 t2) = TypeArrow (applySubsts s t1) (applySubsts s t2)
applySubsts s (TypeApp t1 t2) = TypeApp (applySubsts s t1) (applySubsts s t2)
applySubsts s (TypeForall j t) = TypeForall j $ applySubsts (M.delete j s) t

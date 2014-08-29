{-# LANGUAGE FlexibleInstances #-}

module Language.Haskell.Exference.ConstrainedType
  ( HsConstrainedType (..)
  , readConstrainedType
  , parseConstrainedType
  )
where



import Language.Haskell.Exference.Type
import Language.Haskell.Exference.TypeClasses
import Data.List ( intersperse, find )
import Data.Maybe ( fromMaybe )

import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Char

import Data.Char ( isLower, isUpper )

import Control.Applicative ( (<$>), (<*>), (*>), (<*) )

import Debug.Hood.Observe



data HsConstrainedType = HsConstrainedType [Constraint] HsType
  deriving (Ord, Eq)

instance Show HsConstrainedType where
  showsPrec _ (HsConstrainedType cs t) =
    if null cs
      then shows t
      else 
          showParen True (\x -> foldr (++) x $ intersperse ", " $ map show cs)
        . showString " => "
        . shows t

instance Observable HsConstrainedType where
  observer state = observeOpaque (show state) state

readConstrainedType :: StaticContext -> String -> HsConstrainedType
readConstrainedType c s = case parseConstrainedType c s of
  Just (x,[]) -> x
  _ -> error "readHsConstrainedType: no parse"

parseConstrainedType :: StaticContext
                     -> String
                     -> Maybe (HsConstrainedType, String)
parseConstrainedType c s = either (const Nothing) Just
                         $ runParser (    (,)
                                      <$> constrainedTypeParser c
                                      <*> many anyChar)
                                     ()
                                     ""
                                     s

constrainedTypeParser :: StaticContext -> Parser HsConstrainedType
constrainedTypeParser c = spaces *> 
  ( try (HsConstrainedType <$> parseConstraints
                           <*> (spaces *> string "=>" *> spaces *> typeParser))
  <|>
    HsConstrainedType [] <$> typeParser
  )
  where
    parseConstraints =
      try (char '(' *> spaces *>
           sepBy parseConstraint (spaces >> string "," >> spaces)
           <* spaces <* char ')'
          )
      <|>
          sepBy1 parseConstraint (spaces >> string "," >> spaces)
    parseConstraint :: Parser Constraint
    parseConstraint = Constraint
      <$> do
        cstr <- (:) <$> satisfy isUpper <*> many alphaNum
        return $ fromMaybe unknownTypeClass $ find ((cstr ==) . tclass_name) $ context_tclasses c
      <*> many1 typeParser

constrainedTypeApplySubsts :: Substs -> HsConstrainedType -> HsConstrainedType
constrainedTypeApplySubsts ss (HsConstrainedType cs t) =
  HsConstrainedType
    (map (constraintApplySubsts ss) cs)
    (applySubsts ss t)

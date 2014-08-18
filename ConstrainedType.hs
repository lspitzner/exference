{-# LANGUAGE FlexibleInstances #-}

module ConstrainedType
  ( HsConstrainedType (..)
  , readConstrainedType
  )
where



import Type
import TypeClasses
import Data.List ( intersperse, find )
import Data.Maybe ( fromMaybe )

import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Char

import Data.Char ( isLower, isUpper )

import Control.Applicative ( (<$>), (<*>), (*>), (<*) )



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

readConstrainedType :: StaticContext -> String -> HsConstrainedType
readConstrainedType c s = case parseConstrainedType c s of
  Just (x,[]) -> x
  _ -> error "readHsConstrainedType: no parse"

parseConstrainedType :: StaticContext -> String -> Maybe (HsConstrainedType, String)
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
      char '(' *> spaces *>
      sepBy parseConstraint (spaces >> string "," >> spaces)
      <* spaces <* char ')'
    parseConstraint :: Parser Constraint
    parseConstraint = Constraint
      <$> ( do 
        cstr <- (:) <$> satisfy isUpper <*> many alphaNum
        case find ((cstr ==) . tclass_name) $ context_tclasses c of
          Nothing -> fail ""
          Just x -> return x
        )
      <*> many1 typeParser

constrainedTypeApplySubsts :: Substs -> HsConstrainedType -> HsConstrainedType
constrainedTypeApplySubsts ss (HsConstrainedType cs t) =
  HsConstrainedType
    (map (constraintApplySubsts ss) cs)
    (applySubsts ss t)

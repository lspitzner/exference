module Language.Haskell.Exference.Core.ExferenceStats
  ( ExferenceStats (..)
  , BindingUsages
  )
where

import Data.Map.Strict as M
type BindingUsages = M.Map String Int

data ExferenceStats = ExferenceStats
  { exference_steps :: Int
  , exference_complexityRating :: Float
  , exference_finalSize :: Int
  }
  deriving (Show, Eq)

module Language.Haskell.Exference.ExferenceStats
  ( ExferenceStats (..)
  )
where



data ExferenceStats = ExferenceStats
  { exference_steps :: Int
  , exference_complexityRating :: Float
  }
  deriving Show

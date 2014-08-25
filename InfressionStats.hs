module InfressionStats
  ( InfressionStats (..)
  )
where



data InfressionStats = InfressionStats
  { infression_steps :: Int
  , infression_complexityRating :: Float
  }
  deriving Show

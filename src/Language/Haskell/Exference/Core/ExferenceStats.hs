module Language.Haskell.Exference.Core.ExferenceStats
  ( ExferenceStats (..)
  , BindingUsages
  , emptyBindingUsages
  , incBindingUsage
  )
where



import Data.Map.Strict as M



type BindingUsages = M.Map String Int

emptyBindingUsages :: BindingUsages
emptyBindingUsages = M.empty

incBindingUsage :: String -> BindingUsages -> BindingUsages
incBindingUsage s m = case M.lookup s m of
  Nothing -> M.insert s 1     m
  Just b  -> M.insert s (b+1) m

data ExferenceStats = ExferenceStats
  { exference_steps :: Int
  , exference_complexityRating :: Float
  , exference_finalSize :: Int
  }
  deriving (Show, Eq)

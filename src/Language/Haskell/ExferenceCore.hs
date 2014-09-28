module Language.Haskell.ExferenceCore
  ( findExpressions
  , findExpressionsChunked
  , findExpressionsWithStats
  , E.ExferenceHeuristicsConfig (..)
  , E.ExferenceInput (..)
  , E.ExferenceOutputElement
  )
where



import qualified Language.Haskell.Exference.Internal.Exference as E
import qualified Language.Haskell.Exference.ExferenceStats as S


findExpressions :: E.ExferenceInput -> [E.ExferenceOutputElement]
findExpressions = concat . map snd . E.findExpressions

findExpressionsChunked :: E.ExferenceInput
                   -> [[E.ExferenceOutputElement]]
findExpressionsChunked = map snd . E.findExpressions

findExpressionsWithStats :: E.ExferenceInput
                         -> [(S.BindingUsages, [E.ExferenceOutputElement])]
findExpressionsWithStats = E.findExpressions

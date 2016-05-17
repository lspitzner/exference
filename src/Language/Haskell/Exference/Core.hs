module Language.Haskell.Exference.Core
  ( findExpressions
  , findExpressionsChunked
  , findExpressionsWithStats
  , E.ExferenceHeuristicsConfig (..)
  , E.ExferenceInput (..)
  , E.ExferenceOutputElement
  , E.ExferenceChunkElement (..)
  )
where



import qualified Language.Haskell.Exference.Core.Internal.Exference as E
import qualified Language.Haskell.Exference.Core.ExferenceStats as S
import qualified Language.Haskell.Exference.Core.SearchTree as ST
import Control.Monad ( join )
import Data.Monoid ( mempty )



findExpressions :: E.ExferenceInput -> [E.ExferenceOutputElement]
findExpressions = concatMap E.chunkElements . E.findExpressions

findExpressionsChunked :: E.ExferenceInput
                   -> [[E.ExferenceOutputElement]]
findExpressionsChunked = map E.chunkElements . E.findExpressions

findExpressionsWithStats :: E.ExferenceInput
                         -> [E.ExferenceChunkElement]
findExpressionsWithStats = E.findExpressions

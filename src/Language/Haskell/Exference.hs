{-# LANGUAGE PatternGuards #-}

module Language.Haskell.Exference
  ( findExpressions
  , findExpressionsPar
  , findOneExpression
  , findOneExpressionPar
  , findSortNExpressions
  , findBestNExpressions
  , findFirstBestExpressions
  , takeFindSortNExpressions
  , findFirstExpressionLookahead
  , ExferenceInput ( .. )
  , ExferenceOutputElement
  , ExferenceStats (..)
  )
where



import Language.Haskell.ExferenceCore

import Language.Haskell.Exference.ExferenceStats

import Data.Maybe ( maybeToList, listToMaybe, fromMaybe )
import Control.Arrow ( first, second, (***) )
import Control.Monad ( guard, mzero )
import Control.Applicative ( (<$>), (<*>) )
import Data.List ( partition, sortBy, groupBy, minimumBy )
import Data.Ord ( comparing )
import Data.Function ( on )

import qualified ListT ( ListT, take, head, uncons, toList )



-- returns the first found solution (not necessarily the best overall)
findOneExpression :: ExferenceInput
                  -> Maybe ExferenceOutputElement
findOneExpression input = listToMaybe $ findExpressions input

findOneExpressionPar :: ExferenceInput
                     -> IO (Maybe ExferenceOutputElement)
-- findOneExpressionPar input = findExpressionsPar input $ ListT.head
findOneExpressionPar input = if input_maxSteps input < 1024
  then return $ findOneExpression input
  else case findOneExpression $ input { input_maxSteps = 512 } of
    Nothing -> findExpressionsChunkedPar input f
    x -> return x
  where
    f :: ListT.ListT IO [ExferenceOutputElement]
      -> IO (Maybe ExferenceOutputElement)
    f listt = do
      x <- ListT.uncons listt
      case x of
        Nothing             -> return Nothing
        Just ([], rest)     -> f rest
        Just (elems1, rest) -> do
          elemsR <- ListT.toList $ ListT.take 64 rest
          return $ Just
                 $ minimumBy (comparing (exference_complexityRating.snd))
                 $ concat
                 $ elems1:elemsR


-- calculates at most n solutions, sorts by rating, returns the first m
takeFindSortNExpressions :: Int
                         -> Int
                         -> ExferenceInput
                         -> [ExferenceOutputElement]
takeFindSortNExpressions m n input =
  take m $ findSortNExpressions n input

-- calculates at most n solutions, and returns them sorted by their rating
findSortNExpressions :: Int
                     -> ExferenceInput
                     -> [ExferenceOutputElement]
findSortNExpressions n input = sortBy (comparing g) $ take n $ r
  where
    r = findExpressions input
    g (_,ExferenceStats _ f) = f

-- returns the first expressions with the best rating.
-- best explained on examples:
--   []      -> []
--   [2,5,5] -> [2]
--   [3,3,3,4,4,5,6,7] -> [3,3,3]
--   [2,5,2] -> [2] -- will not look past worse ratings
--   [4,3,2,2,2,3] -> [2,2,2] -- if directly next is better, switch to that
findFirstBestExpressions :: ExferenceInput
                         -> [ExferenceOutputElement]
findFirstBestExpressions input
  | r <- findExpressions input
  , f <- head . groupBy ((>=) `on` exference_complexityRating.snd)
  = case r of
    [] -> []
    _  -> f $ reverse $ f $ r

-- tries to find the best solution by performing a limitted amount of steps,
-- resetting the count whenever a better solution is found.
-- "finds the "first" (by some metric) local maximum"
-- advantages:
--   - might be able to find a "best" solution quicker than other approaches
--   - does not calculate the maximum amount of steps when there is no
--     solution left.
-- disadvantages:
--   - might find only a local optimum
findFirstExpressionLookahead :: Int
                             -> ExferenceInput
                             -> Maybe ExferenceOutputElement
findFirstExpressionLookahead n input = f n Nothing (findExpressionsChunked input)
  where
    f :: Int
      -> Maybe ExferenceOutputElement
      -> [[ExferenceOutputElement]]
      -> Maybe ExferenceOutputElement
    f _ x        []      = x
    f 0 best     _       = best
    f r best     ([]:sr) = f (r-1) best sr
    f _ Nothing  (s:sr)  = f n (Just $ minElem s) sr
    f r (Just b) (s:sr)  | sbest <- minElem s
                         , exference_complexityRating (snd sbest)
                         < exference_complexityRating (snd b)
                         = f n (Just sbest) sr
                         | otherwise = f (r-1) (Just b) sr
    minElem :: [ExferenceOutputElement] -> ExferenceOutputElement
    minElem = minimumBy (comparing (exference_complexityRating.snd))

-- like findSortNExpressions, but retains only the best rating
findBestNExpressions :: Int
                     -> ExferenceInput
                     -> [ExferenceOutputElement]
findBestNExpressions n input
  | r <- findSortNExpressions n input
  = case r of
    [] -> []
    _  -> head $ groupBy ((>=) `on` exference_complexityRating.snd) $ r

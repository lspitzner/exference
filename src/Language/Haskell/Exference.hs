{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}

module Language.Haskell.Exference
  ( findExpressions
  , findOneExpression
  , findSortNExpressions
  , findBestNExpressions
  , findFirstBestExpressions
  , takeFindSortNExpressions
  , findFirstExpressionLookahead
  , findFirstBestExpressionsLookahead
  , findFirstBestExpressionsLookaheadPreferNoConstraints
  , ExferenceInput ( .. )
  , ExferenceOutputElement
  , ExferenceStats (..)
  )
where



import Language.Haskell.Exference.Core

import Language.Haskell.Exference.Core.ExferenceStats

import Data.Maybe ( maybeToList, listToMaybe, fromMaybe )
import Control.Arrow ( first, second, (***) )
import Control.Monad ( guard, mzero )
import Control.Applicative ( (<$>), (<*>) )
import Data.List ( partition, sortBy, groupBy, minimumBy )
import Data.Ord ( comparing )
import Data.Function ( on )

import Debug.Trace



-- returns the first found solution (not necessarily the best overall)
findOneExpression :: ExferenceInput
                  -> Maybe ExferenceOutputElement
findOneExpression = listToMaybe . findExpressions

-- calculates at most n solutions, sorts by rating, returns the first m
takeFindSortNExpressions :: Int
                         -> Int
                         -> ExferenceInput
                         -> [ExferenceOutputElement]
takeFindSortNExpressions m n =
  take m . findSortNExpressions n

-- calculates at most n solutions, and returns them sorted by their rating
findSortNExpressions :: Int
                     -> ExferenceInput
                     -> [ExferenceOutputElement]
findSortNExpressions n input = sortBy (comparing g) $ take n $ r
  where
    r = findExpressions input
    g (_, _, ExferenceStats _ f _) = f

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
  , f <- head . groupBy (\(~(_, _, stats1)) (~(_, _, stats2)) -> 
                              exference_complexityRating stats1
                           >= exference_complexityRating stats2)
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
findFirstExpressionLookahead n = f 999999 Nothing . findExpressionsChunked
  where
    f :: Int
      -> Maybe ExferenceOutputElement
      -> [[ExferenceOutputElement]]
      -> Maybe ExferenceOutputElement
    f _ best     []      = best
    f 0 best     _       = best
    f r best     ([]:sr) = f (r-1) best sr
    f _ Nothing  (s:sr)  = f n (Just $ minElem s) sr
    f r (Just (b@(_, _, statsB))) (s:sr) 
                         | sbest@(_, _, statsBest) <- minElem s
                         , exference_complexityRating statsBest
                         < exference_complexityRating statsB
                         = f n (Just sbest) sr
                         | otherwise = f (r-1) (Just b) sr
    minElem :: [ExferenceOutputElement] -> ExferenceOutputElement
    minElem = minimumBy (\(~(_, _, stats1)) (~(_, _, stats2)) ->
                           compare (exference_complexityRating stats1)
                                   (exference_complexityRating stats2))

-- a combination of the return-multiple-if-same-rating and the
-- look-some-steps-ahead-for-better-solution functionalities.
-- for example,
-- [2,3,2,2,4,5,6,7] -> [2,2,2]
--  does not stop at 3, but looks ahead, then returns all the 2-rated solutions
findFirstBestExpressionsLookahead :: Int
                                  -> ExferenceInput
                                  -> [ExferenceOutputElement]
findFirstBestExpressionsLookahead n =
  f 999999 99999.9 [] . findExpressionsChunked
 where
  f :: Int
    -> Float
    -> [ExferenceOutputElement]
    -> [[ExferenceOutputElement]]
    -> [ExferenceOutputElement]
  f _ _ ss []           = ss
  f 0 _ ss _            = ss
  f i r ss ([]:qss)     = f (i-1) r ss qss
  f i r ss ((q@(_, _, statsQ):qs):qss)
                        | rq <- exference_complexityRating statsQ
                        = if
                        | rq < r    -> f n rq [q] (qs:qss)
                        | rq == r   -> f n r (q:ss) (qs:qss)
                        | otherwise -> f i r ss (qs:qss)

-- a combination of the return-multiple-if-same-rating and the
-- look-some-steps-ahead-for-better-solution functionalities.
-- for example,
-- [2,3,2,2,4,5,6,7] -> [2,2,2]
--  does not stop at 3, but looks ahead, then returns all the 2-rated solutions
findFirstBestExpressionsLookaheadPreferNoConstraints :: Int
                                                     -> ExferenceInput
                                                     -> [ExferenceOutputElement]
findFirstBestExpressionsLookaheadPreferNoConstraints n =
  f 999999 99999.9 [] [] . findExpressionsChunked
 where
  f :: Int
    -> Float
    -> [ExferenceOutputElement] -- solutions without constraints
    -> [ExferenceOutputElement] -- solution(s) with constraints
    -> [[ExferenceOutputElement]]
    -> [ExferenceOutputElement]
  -- out of potential solutions, nothing constraint-free found
  f _ _ [] ssc []           = ssc
  -- out of potential solutions, found good stuff
  f _ _ ss _   []           = ss
  -- out of lookahead, return what we have (ss wont be null)
  f 0 _ ss _   _            = ss
  -- simple reduce when no good solutions yet
  f i r [] ssc ([]:qss)     = f i r [] ssc qss
  -- lookahead step when we already have some good solution
  f i r ss ssc ([]:qss)     = f (i-1) r ss ssc qss
  -- finding one/the first good solution
  f i r ss _   ((q@(_, [], statsQ):qs):qss)
                        | rq <- exference_complexityRating statsQ
                        = if
                        | null ss   -> f n rq [q] [] (qs:qss)
                        | rq < r    -> f n rq [q] [] (qs:qss)
                        | rq == r   -> f n r (q:ss) [] (qs:qss)
                        | otherwise -> f i r ss [] (qs:qss)
  -- finding a bad solution when there are no good solutions yet
  f i r [] ssc ((q@(_, _, statsQ):qs):qss)
                        | rq <- exference_complexityRating statsQ
                        = if
                        | rq < r    -> f i rq [] [q] (qs:qss)
                        | rq == r   -> f i r [] (q:ssc) (qs:qss)
                        | otherwise -> f i r [] ssc (qs:qss)
  -- finding a bad solution when we already have good solutions
  f i r ss _   ((_:qs):qss) = f (i-1) r ss [] (qs:qss)

-- like findSortNExpressions, but retains only the best rating
findBestNExpressions :: Int
                     -> ExferenceInput
                     -> [ExferenceOutputElement]
findBestNExpressions n input
  | r <- findSortNExpressions n input
  = case r of
    [] -> []
    _  -> head $ groupBy (\(~(_, _, stats1)) (~(_, _, stats2)) -> 
                              exference_complexityRating stats1
                           >= exference_complexityRating stats2)
                         r

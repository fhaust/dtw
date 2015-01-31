
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module implements dynamic time warping as described here:
-- http://en.wikipedia.org/w/index.php?title=Dynamic_time_warping&oldid=643501828
--
-- Additionally 'fastDtw' is implemented as described in the paper:
-- "FastDTW: Toward Accurate Dynamic Time Warping in Linear Time and
-- Space" by Stan Salvador and Philip Chan.
--
-- Please note that 'fastDtw' is only an approximative solution. If you
-- need the optimal solution and can bear with the heavily increased demand
-- both in cpu and in memory you should use 'dtwMemo' or 'dtwMemoWindowed'.
--
--
-- == Example
--
-- >>> -- create two sample datasets
-- >>> let as = [ sin x | x <- [0,0.1..pi] ]
-- >>> let bs = [ sin (x+0.1) | x <- [0,0.1..pi] ]
-- >>> -- define a cost function between two datapoints
-- >>> let dist x y = abs (x-y)
-- >>> -- define a function that will half the size of a dataset (see below)
-- >>> let shrink xs = case xs of (a:b:cs) -> (a+b)/2 : shrink cs; a:[] -> [a]; [] -> []
-- >>> -- calculate the cost with fastDtw and dtwMemo for comparison
-- >>> cost $ fastDtw dist shrink 2 as bs :: Float
-- 0.19879311
-- >>> cost $ dtwMemo (\x y -> abs (x-y)) as bs :: Float
-- 0.19879311
--
-- == Some words on the shrink function
--
-- Care must be taken when choosing a shrink function. It's vital that the
-- resolution is halfed, this is not exactly a problem with the algorithm
-- but with the implementation. The lower resolution dataset should be an
-- honest representation of the higher resolution dataset. For starters
-- binning as in the example above should suffice.
-- 


module Data.DTW (dtwNaive, dtwMemo, fastDtw, Result(..), Path, Index) where


import           Data.Sequence (Seq, ViewL (..), (<|))
import qualified Data.Sequence as S

import qualified Data.Set as Set
import qualified Data.List as L

import           Data.MemoTrie
import           Data.Function



-- | a generic dataset is basically just an indexing function
-- | and an indicator of the dataset size
class DataSet dataset  where
    type Item dataset :: *
    ix  :: dataset -> Int -> Item dataset
    len :: dataset -> Int

-- some DataSet orphan instances
instance DataSet (S.Seq a) where
    type Item (S.Seq a) = a
    ix  = S.index
    len = S.length

instance DataSet [a] where
    type Item [a] = a
    ix  = (!!)
    len = length

-- common types

type Index  = (Int,Int)
type Path   = [Index]
type Window = Set.Set Index

data Result a = Result { cost :: a, path :: Path }


-- | this is the naive implementation of dynamic time warping
-- no caching what so ever is taking place
-- this should not be used and is just used as a reference for the other
-- implementations

dtwNaive :: (Ord c, Fractional c, DataSet a, DataSet b)
         => (Item a -> Item b -> c) -> a -> b -> c
dtwNaive δ as bs = go (len as - 1) (len bs - 1)
    where go 0 0 = 0
          go _ 0 = 1/0
          go 0 _ = 1/0
          go x y = δ (ix as x) (ix bs y) + minimum [ go (x-1)  y
                                                   , go  x    (y-1)
                                                   , go (x-1) (y-1)
                                                   ]

-------------------------------------------------------------------------------------

-- | this is the "standard" implementation of dynamic time warping
-- O(N^2) is achieved by memoization of previous results
dtwMemo :: (Ord c, Fractional c, DataSet a, DataSet b)
        => (Item a -> Item b -> c) -> a -> b -> Result c
dtwMemo δ = dtwMemoWindowed δ (\_ _ -> True)

{-# INLINABLE dtwMemo #-}

-- | "standard" implementation of dynamic time warping with an additional
-- parameter that can be used to define a search window
dtwMemoWindowed :: (Ord c, Fractional c, DataSet a, DataSet b)
                => (Item a -> Item b -> c)
                -> (Int -> Int -> Bool)
                -> a
                -> b
                -> Result c
dtwMemoWindowed δ inWindow as bs = go (len as - 1) (len bs - 1)
    where -- wrap go' in a memoziation function so that each value
          -- is calculated only once
          go = memo2 go'
          -- handle special cases, origin cost is zero,
          -- border cost is infinity
          go' 0 0                      = Result 0 [(0,0)]
          go' 0 y                      = Result (1/0) [(0,y)]
          go' x 0                      = Result (1/0) [(x,0)]
          -- check that this index is not out of the search window
          go' x y | not (inWindow x y) = Result (1/0) [(x,y)]
          -- else calculate this value, note that this calls the
          -- memoized version of go recursivly
          go' x y                      = Result newCost newPath
            where minResult = L.minimumBy (compare `on` cost) [ go (x-1)  y
                                                              , go  x    (y-1)
                                                              , go (x-1) (y-1) ]
                  newPath   = (x,y) : path minResult
                  newCost   = δ (ix as x) (ix bs y) + cost minResult

{-# INLINABLE dtwMemoWindowed #-}

-------------------------------------------------------------------------------------

{--- | reduce a dataset to half its size by averaging neighbour values-}
{--- together-}
{-reduceByHalf :: Fractional a => Seq a -> Seq a-}
{-reduceByHalf (S.viewl -> x :< (S.viewl -> y :< xs))  = (x + y) / 2 <| reduceByHalf xs-}
{-reduceByHalf (S.viewl -> x :< (S.viewl -> S.EmptyL)) = S.singleton x-}
{-reduceByHalf _                                       = S.empty-}

-- | create a search window by projecting the path from a lower resolution
-- to the next level as defined in the fastdtw paper (figure 6), ie:
--
-- +-------+-------+        +---+---+---+---+
-- |       |       |        |   |   | X | X |
-- |       |   X   |        +---+---+---+---+
-- |       |       |        |   | X | X | X |
-- +-------+-------+   ->   +---+---+---+---+
-- |       |       |        | X | X | X |   |
-- |   X   |       |        +---+---+---+---+
-- |       |       |        | X | X |   |   |
-- +-------+-------+        +---+---+---+---+
projectPath :: Path -> Window
projectPath p = Set.fromList $ concatMap expand $ concat $ zipWith project p (tail p)
  where project (a,b) (c,d) = [(2*a,2*b),((2*a+2*c) `quot` 2, (2*b+2*d) `quot` 2), (2*c,2*d)]
        expand  (a,b)       = [(a,b),(a+1,b),(a,b+1),(a+1,b+1)]

-- | expand the search window by a given radius
-- (compare fastdtw paper figure 6)
-- ie for a radius of 1:
--
-- +---+---+---+---+        +---+---+---+---+
-- |   |   | X | X |        | o | o | X | X |
-- +---+---+---+---+        +---+---+---+---+
-- |   | X | X | X |        | o | X | X | X |
-- +---+---+---+---+   ->   +---+---+---+---+
-- | X | X | X |   |        | X | X | X | o |
-- +---+---+---+---+        +---+---+---+---+
-- | X | X |   |   |        | X | X | o | o |
-- +---+---+---+---+        +---+---+---+---+
--
-- the "o"s mark the expanded regions
expandWindow :: Int -> Window -> Window
expandWindow r = Set.fromList . concatMap (\(x,y) -> [ (x',y') | y' <- [y-r..y+r], x' <- [x-r..x+r] ]) . Set.toList



-- | this is the "fast" implementation of dynamic time warping
-- as per the authors this methods calculates a good approximate
-- result in O(N), depending on the usecase the windowsize should be
-- tweaked

fastDtw :: (Ord c, Fractional c, DataSet a)
        => (Item a -> Item a -> c)
        -> (a -> a) -- ^ function that shrinks a dataset by a factor of two
        -> Int      -- ^ radius that the search window is expanded at each resolution level
        -> a        -- ^ first dataset
        -> a        -- ^ second dataset
        -> Result c -- ^ result
fastDtw δ shrink r as bs | len as <= minTSsize || len bs <= minTSsize = dtwMemo δ as bs
                         | otherwise = dtwMemoWindowed δ inWindow as bs
    where minTSsize    = r+2
          shrunkAS     = shrink as
          shrunkBS     = shrink bs
          lowResResult = fastDtw δ shrink r shrunkAS shrunkBS
          window       = expandWindow r $ projectPath (path lowResResult)
          inWindow x y = (x,y) `Set.member` window

{-# INLINABLE fastDtw #-}

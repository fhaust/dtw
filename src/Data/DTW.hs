

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Data.DTW (dtwNaive, dtwMemo, fastDtw, Result(..), Path, Index) where


import           Data.Sequence (Seq, ViewL (..), ViewR (..), (<|), (|>))
import qualified Data.Sequence as S

import qualified Data.Set as Set
import qualified Data.List as L

import           Data.MemoTrie
import           Data.Function


-- common types

type Index  = (Int,Int)
type Path   = [Index]
type Window = Set.Set Index

data Result a = Result { cost :: a, path :: Path }


-- | this is the naive implementation of dynamic time warping
-- no caching what so ever is taking place
-- this should not be used and is just used as a reference for the other
-- implementations

dtwNaive :: (Ord c, Fractional c) => (a -> b -> c) -> S.Seq a -> S.Seq b -> c
dtwNaive _ as bs | S.length as <= 1 && S.length bs <= 1 = 0
dtwNaive _ as bs | S.length as <= 1 || S.length bs <= 1 = 1/0
dtwNaive δ (S.viewr -> as :> a) (S.viewr -> bs :> b) = δ a b + minimum [ dtwNaive δ (as |> a) bs
                                                                       , dtwNaive δ as (bs |> b)
                                                                       , dtwNaive δ as bs
                                                                       ]
dtwNaive _ _ _ = 1/0


-------------------------------------------------------------------------------------

-- | this is the "standard" implementation of dynamic time warping
-- O(N^2) is achieved by memoization of previous results
dtwMemo :: (Ord c, Fractional c) => (a -> b -> c) -> S.Seq a -> S.Seq b -> Result c
dtwMemo δ = dtwMemoWindowed δ (\_ _ -> True)

{-# INLINABLE dtwMemo #-}

-- | "standard" implementation of dynamic time warping with an additional
-- parameter that can be used to define a search window
dtwMemoWindowed :: (Ord c, Fractional c) => (a -> b -> c) -> (Int -> Int -> Bool) -> S.Seq a -> S.Seq b -> Result c
dtwMemoWindowed δ inWindow as bs = go (S.length as - 1) (S.length bs - 1)
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
            where minResult = L.minimumBy (compare `on` cost) [ go (x-1) y, go x (y-1), go (x-1) (y-1) ]
                  newPath   = (x,y) : path minResult
                  newCost   = δ (S.index as x) (S.index bs y) + cost minResult

{-# INLINABLE dtwMemoWindowed #-}

-------------------------------------------------------------------------------------

-- | reduce a dataset to half its size by averaging neighbour values
-- together
reduceByHalf :: Fractional a => Seq a -> Seq a
reduceByHalf (S.viewl -> x :< (S.viewl -> y :< xs))  = (x + y) / 2 <| reduceByHalf xs
reduceByHalf (S.viewl -> x :< (S.viewl -> S.EmptyL)) = S.singleton x
reduceByHalf _                                       = S.empty

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

fastDtw :: (Fractional a, Fractional b, Ord c, Fractional c)
        => (a -> b -> c) -> Int -> Seq a -> Seq b -> Result c
fastDtw δ r as bs | S.length as <= minTSsize || S.length bs <= minTSsize = dtwMemo δ as bs
                  | otherwise = dtwMemoWindowed δ inWindow as bs
    where minTSsize    = r+2
          shrunkAS     = reduceByHalf as
          shrunkBS     = reduceByHalf bs
          lowResResult = fastDtw δ r shrunkAS shrunkBS
          window       = expandWindow r $ projectPath (path lowResResult)
          inWindow x y = (x,y) `Set.member` window

{-# INLINABLE fastDtw #-}

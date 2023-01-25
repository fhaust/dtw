
module Main where



-- module under test
import Data.DTW

import Data.List

import Data.Functor

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.QuickCheck

import qualified Data.Vector.Unboxed as V

newtype SmallNonEmptySeq a = SmallNonEmptySeq { getSmallNonEmpty :: V.Vector a }
    deriving (Show, Eq)

instance (V.Unbox a, Arbitrary a) => Arbitrary (SmallNonEmptySeq a) where
    arbitrary = SmallNonEmptySeq . V.fromList <$> listOf arbitrary `suchThat` (\l -> length l > 2 && length l < 10)

newtype MediumNonEmptySeq a = MediumNonEmptySeq { getMediumNonEmpty :: V.Vector a }
    deriving (Show, Eq)

instance (V.Unbox a, Arbitrary a) => Arbitrary (MediumNonEmptySeq a) where
    arbitrary = MediumNonEmptySeq . V.fromList <$> listOf arbitrary `suchThat` (\l -> length l > 100 && length l < 1000)


dist :: Double -> Double -> Double
dist x y = abs (x-y)

-- | reduce a dataset to half its size by averaging neighbour values
-- together
reduceByHalf :: (V.Unbox a, Fractional a) => V.Vector a -> V.Vector a
reduceByHalf v | V.null v        = V.empty
               | V.length v == 1 = V.singleton (V.head v)
               | even (V.length v) = split v
               | otherwise         = split v `V.snoc` V.last v
    where split w = V.generate (V.length w `div` 2) (\i -> (w V.! (i*2+0)) + (w V.! (i*2+1)))

{-testDTWVSDTWNaive :: (SmallNonEmptySeq Double, SmallNonEmptySeq Double) -> Bool-}
{-testDTWVSDTWNaive (la,lb) = abs (dtwNaive dist sa sb - dtw dist sa sb) < 0.01-}
{-  where sa = S.fromList $ getSmallNonEmpty la-}
{-        sb = S.fromList $ getSmallNonEmpty lb-}

testDTWMemoVSDTWNaive :: (SmallNonEmptySeq Double, SmallNonEmptySeq Double) -> Bool
testDTWMemoVSDTWNaive (la,lb) = abs (dtwNaive dist sa sb - cost (dtwMemo dist sa sb)) < 0.01
  where sa = getSmallNonEmpty la
        sb = getSmallNonEmpty lb

testFastDTWvsDTWNaive :: (SmallNonEmptySeq Double, SmallNonEmptySeq Double) -> Bool
testFastDTWvsDTWNaive (la,lb) = abs (1 - (ca/l) / (cb/l)) < 0.1
  where sa = getSmallNonEmpty la
        sb = getSmallNonEmpty lb
        l  = fromIntegral $ V.length sa + V.length sb
        ca = dtwNaive dist sa sb
        cb = cost $ fastDtw dist reduceByHalf 2 sa sb

-- FIXME no real idea how to compare an optimal and an approximative
-- algorithm ... best bet below, but still failing tests
testFastDTWvsDTWMemoErr :: Int -> (MediumNonEmptySeq Double, MediumNonEmptySeq Double) -> Double
testFastDTWvsDTWMemoErr radius (la,lb) = err
  where sa      = getMediumNonEmpty la
        sb      = getMediumNonEmpty lb
        optimal = cost (dtwMemo dist sa sb)
        approx  = cost (fastDtw dist reduceByHalf radius sa sb)
        err     = (approx - optimal) / optimal * 100

testFastDTWvsDTWMemo :: Int -> Double -> Property
testFastDTWvsDTWMemo radius goal = forAll (vector 25) go
    where go xs = median < goal
            where errs = map (testFastDTWvsDTWMemoErr radius) xs
                  median = sort errs !! (length errs `div` 2)

main :: IO ()
main = defaultMain 
     [ testProperty "dtwMemo ≡ dtwNaive" testDTWMemoVSDTWNaive
     {-, testProperty "fastDtw ≅ dtwNaive" testFastDTWvsDTWNaive-}
     -- , testProperty "fastDtw == dtwMemo (radius=5, maxError=5%)" (testFastDTWvsDTWMemo 5 5)
     ]

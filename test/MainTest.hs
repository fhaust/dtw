
{-# LANGUAGE ViewPatterns #-}

module Main where



-- module under test
import Data.DTW

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.QuickCheck

import           Data.Sequence (Seq(..), ViewL(..), (<|))
import qualified Data.Sequence as S

import Data.Functor


newtype SmallNonEmptySeq a = SmallNonEmptySeq { getSmallNonEmpty :: [a] }
    deriving (Show, Eq)

instance Arbitrary a => Arbitrary (SmallNonEmptySeq a) where
    arbitrary = SmallNonEmptySeq <$> listOf arbitrary `suchThat` (\l -> length l > 2 && length l < 10)

newtype MediumNonEmptySeq a = MediumNonEmptySeq { getMediumNonEmpty :: [a] }
    deriving (Show, Eq)

instance Arbitrary a => Arbitrary (MediumNonEmptySeq a) where
    arbitrary = MediumNonEmptySeq <$> listOf arbitrary `suchThat` (\l -> length l > 100 && length l < 1000)


dist :: Double -> Double -> Double
dist x y = abs (x-y)

-- | reduce a dataset to half its size by averaging neighbour values
-- together
reduceByHalf :: Fractional a => Seq a -> Seq a
reduceByHalf (S.viewl -> x :< (S.viewl -> y :< xs))  = (x + y) / 2 <| reduceByHalf xs
reduceByHalf (S.viewl -> x :< (S.viewl -> S.EmptyL)) = S.singleton x
reduceByHalf _                                       = S.empty

{-testDTWVSDTWNaive :: (SmallNonEmptySeq Double, SmallNonEmptySeq Double) -> Bool-}
{-testDTWVSDTWNaive (la,lb) = abs (dtwNaive dist sa sb - dtw dist sa sb) < 0.01-}
{-  where sa = S.fromList $ getSmallNonEmpty la-}
{-        sb = S.fromList $ getSmallNonEmpty lb-}

testDTWMemoVSDTWNaive :: (SmallNonEmptySeq Double, SmallNonEmptySeq Double) -> Bool
testDTWMemoVSDTWNaive (la,lb) = abs (dtwNaive dist sa sb - cost (dtwMemo dist sa sb)) < 0.01
  where sa = S.fromList $ getSmallNonEmpty la
        sb = S.fromList $ getSmallNonEmpty lb

testFastDTWvsDTWNaive :: (SmallNonEmptySeq Double, SmallNonEmptySeq Double) -> Bool
testFastDTWvsDTWNaive (la,lb) = abs (1 - (ca/l) / (cb/l)) < 0.1
  where sa = S.fromList $ getSmallNonEmpty la
        sb = S.fromList $ getSmallNonEmpty lb
        l  = fromIntegral $ S.length sa + S.length sb
        ca = dtwNaive dist sa sb
        cb = cost $ fastDtw dist reduceByHalf 2 sa sb

-- FIXME no real idea how to compare an optimal and an approximative
-- algorithm ... best bet below, but still failing tests
{-testFastDTWvsDTWMemo :: (MediumNonEmptySeq Double, MediumNonEmptySeq Double) -> Bool-}
{-testFastDTWvsDTWMemo (la,lb) = abs (1 - ((costA / matSize) / (costB / matSize))) < 0.1-}
{-  where sa = S.fromList $ getMediumNonEmpty la-}
{-        sb = S.fromList $ getMediumNonEmpty lb-}
{-        costA = cost (dtwMemo dist sa sb)-}
{-        costB = cost (fastDtw dist 10 sa sb)-}
{-        matSize = fromIntegral $ S.length sa * S.length sb-}

main :: IO ()
main = defaultMain 
     [ testProperty "dtwMemo ≡ dtwNaive" testDTWMemoVSDTWNaive
     , testProperty "fastDtw ≅ dtwNaive" testFastDTWvsDTWNaive
     {-, testProperty "fastDtw == dtwMemo"  testFastDTWvsDTWMemo-}
     ]

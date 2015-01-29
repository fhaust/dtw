


module Main where



-- module under test
import Data.DTW

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.QuickCheck

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

sqDist :: Double -> Double -> Double
sqDist x y = abs (x-y)

{-testDTWVSDTWNaive :: (SmallNonEmptySeq Double, SmallNonEmptySeq Double) -> Bool-}
{-testDTWVSDTWNaive (la,lb) = abs (dtwNaive sqDist sa sb - dtw sqDist sa sb) < 0.01-}
{-  where sa = S.fromList $ getSmallNonEmpty la-}
{-        sb = S.fromList $ getSmallNonEmpty lb-}

testDTWMemoVSDTWNaive :: (SmallNonEmptySeq Double, SmallNonEmptySeq Double) -> Bool
testDTWMemoVSDTWNaive (la,lb) = abs (dtwNaive sqDist sa sb - cost (dtwMemo sqDist sa sb)) < 0.01
  where sa = S.fromList $ getSmallNonEmpty la
        sb = S.fromList $ getSmallNonEmpty lb

testFastDTWvsDTWNaive :: (SmallNonEmptySeq Double, SmallNonEmptySeq Double) -> Bool
testFastDTWvsDTWNaive (la,lb) = abs (dtwNaive sqDist sa sb - cost (fastDtw sqDist 2 sa sb)) < 0.01
  where sa = S.fromList $ getSmallNonEmpty la
        sb = S.fromList $ getSmallNonEmpty lb

-- FIXME no real idea how to compare an optimal and an approximative
-- algorithm ... best bet below, but still failing tests
{-testFastDTWvsDTWMemo :: (MediumNonEmptySeq Double, MediumNonEmptySeq Double) -> Bool-}
{-testFastDTWvsDTWMemo (la,lb) = abs (1 - ((costA / matSize) / (costB / matSize))) < 0.1-}
{-  where sa = S.fromList $ getMediumNonEmpty la-}
{-        sb = S.fromList $ getMediumNonEmpty lb-}
{-        costA = cost (dtwMemo sqDist sa sb)-}
{-        costB = cost (fastDtw sqDist 10 sa sb)-}
{-        matSize = fromIntegral $ S.length sa * S.length sb-}

main :: IO ()
main = defaultMain 
     [ testProperty "dtwMemo == dtwNaive" testDTWMemoVSDTWNaive
     , testProperty "fastDtw == dtwNaive" testFastDTWvsDTWNaive
     {-, testProperty "fastDtw == dtwMemo"  testFastDTWvsDTWMemo-}
     ]
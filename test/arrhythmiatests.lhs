\subsubsection{Tests}

\begin{code}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Arrhythmia
import TestUtils

import Sound.Tidal.Params
import Sound.Tidal.Pattern
import Sound.Tidal.Core
import Sound.Tidal.ParseBP

import Test.Hspec
import Test.QuickCheck
import Data.Ratio

instance (Arbitrary a) => Arbitrary (Pattern a) where
  arbitrary = sized m where
    m n | n < 4 = listToPat . (:[]) <$> arbitrary
    m n = fastCat <$> oneof [ sequence [resize (n `div` 2) arbitrary, resize (n `div` 2) arbitrary]
      , sequence [resize (n `div` 2) arbitrary, resize (n `div` 2) arbitrary, resize (n `div` 2) arbitrary]
      , sequence [resize (n `div` 2) arbitrary, resize (n `div` 2) arbitrary, resize (n `div` 2) arbitrary, resize (n `div` 2) arbitrary] ]

instance (Fractional a, Arbitrary a, Eq a) => Arbitrary (ArcF a) where
  arbitrary = sized m where
    m i = Arc 0 . notZero <$> x where
      x = resize (i `div` 2) arbitrary
      notZero n = if n == 0 then 1 else n

\end{code}

Here, we define two tests for arrhythmia. The first checks that shift the time by 0 returns the same
pattern, and the second tests if the pattern eventually repeats the same cycle.

\begin{code}

main :: IO ()
main = hspec $ do
  describe "Arrhythmia" $ do

    it "should not change the pattern if the time shifts by 0" (
      queryArc (arrhythmiaUnsafe 0 $ s $ parseBP_E "bd bd hh") (Arc 0 1)
      ==
      queryArc (s $ parseBP_E "bd bd hh") (Arc 0 1)
      )

    it "should repeat a cycle every k cycles" $
      property $ \a nt ->
        queryArc (arrhythmiaUnsafe nt $ s $ parseBP_E "bd bd hh") a ==
        queryArc (arrhythmiaUnsafe nt $ s $ parseBP_E "bd bd hh") (newArc a nt)
        where
          newArc :: Arc -> Time -> Arc
          newArc a nu = Arc (arcStart a * (denominator nu % 1)) ((arcStart a * (denominator nu % 1)) + (arcEnd a - arcStart a))

\end{code}

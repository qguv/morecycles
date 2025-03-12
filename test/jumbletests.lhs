\subsubsection{Tests}

\begin{code}
module Main where

import Jumble
import TestUtils

import Sound.Tidal.Pattern
import Sound.Tidal.Core
import Sound.Tidal.Show

import Test.Hspec
import Test.QuickCheck
\end{code}

First, we need to describe how to create arbitrary \texttt{Pattern} instances:

\begin{code}
instance (Arbitrary a) => Arbitrary (Pattern a) where
  arbitrary = sized $ m where
    m n | n < 4 = listToPat <$> (:[]) <$> arbitrary
    m n = fastCat <$> oneof [ sequence [resize (n `div` 2) arbitrary, resize (n `div` 2) arbitrary]
      , sequence [resize (n `div` 2) arbitrary, resize (n `div` 2) arbitrary, resize (n `div` 2) arbitrary]
      , sequence [resize (n `div` 2) arbitrary, resize (n `div` 2) arbitrary, resize (n `div` 2) arbitrary, resize (n `div` 2) arbitrary] ]
\end{code}

We can now define our tests:

\begin{code}

main :: IO ()
main = hspec $ do
  describe "Jumble" $ do

    it "shouldn't change the pattern when the permutation index is zero" $
      property $ \mp p -> compareP (Arc 0 8) (jumble' 0 mp p) (p :: Pattern Int)

    it "shouldn't change the pattern when the whole pattern is masked" $
      property $ \i p -> compareP (Arc 0 1) (jumble' i (listToPat [True]) p) (p :: Pattern Int)

    it "should change the pattern if nothing is masked" $
      compareP (Arc 0 8) (jumble' 1 (listToPat [False]) (listToPat [0, 1])) (listToPat [1, 0 :: Int])

    it "should change the pattern with a complex mask" $
      compareP (Arc 0 1) (jumble' 1 (listToPat [True, False, True, False]) (listToPat [1, 2, 3, 4])) (listToPat [1, 4, 3, 2 :: Int])

    it "shouldn't change the pattern when the permutation index loops around" $
      compareP (Arc 0 1) (jumble' 2 (listToPat [True, False, True, False]) (listToPat [1, 2, 3, 4])) (listToPat [1, 2, 3, 4 :: Int])
\end{code}

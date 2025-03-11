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
    it "jumble' 0 shouldn't change the pattern" $
      property $ \mp -> \p -> compareP (Arc 0 8) (jumble' 0 mp p) (p :: Pattern Bool)
    {-
    it "jumble' shouldn't change the pattern if the whole pattern is masked" $
      property $ \i -> \p -> compareP (Arc 0 8) (jumble' i (listToPat [True]) p) (p :: Pattern Bool)
    -}
\end{code}

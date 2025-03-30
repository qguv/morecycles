\subsubsection{Tests}

\begin{code}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Jumble
import TestUtils

import Sound.Tidal.Pattern
import Sound.Tidal.Core
import Sound.Tidal.ParseBP

import Test.Hspec
import Test.QuickCheck
\end{code}

First, we need to describe how to create arbitrary \texttt{Pattern} instances:

\begin{code}
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

We can now define our tests:

\begin{code}

main :: IO ()
main = hspec $ do
  describe "Jumble" $ do

    it "" $
      property $ \a -> compareP a (jumble' 1 (parseBP_E "[0]") (parseBP_E "[a b]")) (parseBP_E "[b a]" :: Pattern String)
\end {code}

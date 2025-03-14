{-# OPTIONS_GHC -Wno-orphans #-}
\subsubsection{Tests}

\begin{code}
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
\end{code}

We can now define our tests:

\begin{code}

main :: IO ()
main = hspec $ do
  describe "Jumble" $ do

    it "shouldn't change the pattern when the permutation index is zero" $
      property $ \t1 t2 mp p -> compareP (Arc (min t1 t2) (max t1 t2)) (jumble' 0 mp p) (p :: Pattern Int)

    it "shouldn't change the pattern when the whole pattern is masked" $
      property $ \t1 t2 i p -> compareP (Arc (min t1 t2) (max t1 t2)) (jumble' i (parseBP_E "[1]") p) (p :: Pattern Int)

    it "should change the pattern if nothing is masked" $
      property $ \t1 t2 -> compareP (Arc (min t1 t2) (max t1 t2)) (jumble' 1 (parseBP_E "[0]") (parseBP_E "[a b]")) (parseBP_E "[b a]" :: Pattern String)

    it "should change the pattern with a complex mask 1" $
      property $ \t1 t2 -> compareP (Arc (min t1 t2) (max t1 t2)) (jumble' 1 (parseBP_E "[1 0 1 0]") (parseBP_E "[a b c d]")) (parseBP_E "[a d c b]" :: Pattern String)

    it "should change the pattern with a complex mask 2" $
      property $ \t1 t2 -> compareP (Arc (min t1 t2) (max t1 t2)) (jumble' 1 (parseBP_E "[1 0]") (parseBP_E "[a b c d]")) (parseBP_E "[a b d c]" :: Pattern String)

    it "should change the pattern with a complex mask 3" $
      property $ \t1 t2 -> compareP (Arc (min t1 t2) (max t1 t2)) (jumble' 1 (parseBP_E "[0 [1 0]]") (parseBP_E "[a b c d]")) (parseBP_E "[b d c a]" :: Pattern String)

    it "should change the pattern with a complex mask 4" $
      property $ \t1 t2 -> compareP (Arc (min t1 t2) (max t1 t2)) (jumble' 1 (parseBP_E "[1 0 1 0]") (parseBP_E "[bd [hh cp] sd cp]")) (parseBP_E "[bd [cp cp] sd hh]" :: Pattern String)

    it "shouldn't change the pattern when the permutation index loops around" $
      property $ \t1 t2 -> compareP (Arc (min t1 t2) (min t1 t2)) (jumble' 2 (listToPat [True, False, True, False]) (listToPat [1, 2, 3, 4])) (listToPat [1, 2, 3, 4 :: Int])
\end{code}

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
import Data.List

-- apply functions to the first and last elements of a list
mapEnds :: (a -> a) -> (a -> a) -> [a] -> [a]
mapEnds _ _ [] = []
mapEnds f g [x] = [(g . f) x]
mapEnds f g (x:xs) = f x : mapLast xs where
  mapLast [] = [] -- impossible
  mapLast [y] = [g y]
  mapLast (y:ys) = y : mapLast ys

trimEventLeft :: Event a -> Event a
trimEventLeft e@Event{whole=Nothing} = e
trimEventLeft e@Event{whole=Just w, part=(Arc t0 _)} = e{whole=Just w{start=t0}}

trimEventRight :: Event a -> Event a
trimEventRight e@Event{whole=Nothing} = e
trimEventRight e@Event{whole=Just w, part=(Arc _ t1)} = e{whole=Just w{stop=t1}}

trimEvents :: [Event a] -> [Event a]
trimEvents = mapEnds trimEventLeft trimEventRight

-- | Compare the events of two patterns using the given arc, trimming the first and last events
comparePT :: (Ord a, Show a) => Arc -> Pattern a -> Pattern a -> Expectation
comparePT a p p' =
  (trimEvents . sort) (queryArc (stripContext p) a)
    `shouldBe` (trimEvents . sort) (queryArc (stripContext p') a)
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

    it "should change the pattern if nothing is masked" $
      property $ \a -> comparePT a (jumble' 1 (parseBP_E "[0]") (parseBP_E "[a b]")) (parseBP_E "[b a]" :: Pattern String)

    it "should change the pattern with a complex mask 1" $
      property $ \a -> comparePT a (jumble' 1 (parseBP_E "[1 0 1 0]") (parseBP_E "[a b c d]")) (parseBP_E "[a d c b]" :: Pattern String)

    it "should change the pattern with a complex mask 2" $
      property $ \a -> comparePT a (jumble' 1 (parseBP_E "[1 0]") (parseBP_E "[a b c d]")) (parseBP_E "[a b d c]" :: Pattern String)

    it "should change the pattern with a complex mask 3" $
      property $ \a -> comparePT a (jumble' 1 (parseBP_E "[0 [1 0]]") (parseBP_E "[a b c d]")) (parseBP_E "[b d c a]" :: Pattern String)

    it "should change the pattern with a complex mask 4" $
      property $ \a -> comparePT a (jumble' 1 (parseBP_E "[1 0 1 0]") (parseBP_E "[bd [hh cp] sd cp]")) (parseBP_E "[bd [cp cp] sd hh]" :: Pattern String)

    it "shouldn't change the pattern when the permutation index is zero" $
      property $ \a mp p -> comparePT a (jumble' 0 mp p) (p :: Pattern Int)

    it "shouldn't change the pattern when the whole pattern is masked" $
      property $ \a i p -> comparePT a (jumble' i (parseBP_E "[1]") p) (p :: Pattern Int)

    it "shouldn't change the pattern when the permutation index loops around" $
      property $ \a -> comparePT a (jumble' 2 (listToPat [True, False, True, False]) (listToPat [1, 2, 3, 4])) (listToPat [1, 2, 3, 4 :: Int])
\end{code}

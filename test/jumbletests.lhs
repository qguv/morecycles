\subsubsection{Tests}

\begin{code}
module Main where

import Jumble

import Test.Hspec
import Test.QuickCheck

import Sound.Tidal.Pattern
\end{code}

First, we need to describe how to create arbitrary \texttt{Pattern} instances:

\begin{code}
{-
instance Arbitrary Pattern a where
  arbitrary = sized $ m where
    m 0 = pure $ listToPattern [arbitrary]
    m 1 = pure $ listToPattern [arbitrary, arbitrary]
    m n = pure $ listToPattern [resize (n//2) arbitrary, resize (n//2) arbitrary]
-}
\end{code}

We can now define our tests:

\begin{code}
{-
main :: IO ()
main = hspec $ do
  describe "Jumble" $ do
    it "jumble' 0 shouldn't change the pattern" $
      property $ \mp -> \p -> jumble' 0 mp p == p
    it "jumble' shouldn't change the pattern if the whole pattern is masked" $
      property $ \i -> \p -> jumble' i (listToPattern [true]) p == p
-}
\end{code}

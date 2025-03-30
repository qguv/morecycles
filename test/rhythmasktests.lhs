\subsubsection*{Tests}
The test suite validates the behavior of RhythMask, testing the performance of not only the main event manipulation functions, but also the helper functions that are responsible for filtering events and generating masks (Boolean and probabilistic both). The tests are written using \texttt{hspec} and \texttt{QuickCheck}.

\hide{
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Monad (replicateM)
import System.Random (randomRIO)
import RhythMask
import Sound.Tidal.Context
\end{code}
}

\begin{code}
-- Helpers
stateFor :: Int -> State
stateFor numEvents = State (Arc 0 (fromIntegral numEvents)) 0
randomMaskStringIO :: Int -> IO String
randomMaskStringIO n = unwords <$> replicateM n (randomRIO (0 :: Int, 1) >>= \b -> return (show b))
-- Generator: list of length n with alternating True/False
genAlternatingMask :: Int -> Gen [Bool]
genAlternatingMask n = return $ take n (cycle [True, False])
-- Generator: list of length n with alternating 1.0 and 0.0
genAlternatingProbs :: Int -> Gen [Double]
genAlternatingProbs n = return $ take n (cycle [1.0, 0.0])
\end{code}

\texttt{parseMask} validates that \texttt{parseMask} correctly converts a string like "1 0 1 0" into a Pattern Bool with the correct True/False values for one cycle.
\begin{code}
-- Test: parseMask (static)
testParseMask :: Spec
testParseMask = describe "parseMask" $
  it "parses '1 0 1 0' into [True, False, True, False]" $ do
    let maskPat = parseMask "1 0 1 0"
        events  = take 4 (query maskPat (stateFor 4))
        values  = map value events
    values `shouldBe` [True, False, True, False]
\end{code}

\texttt{myFilterEvents} checks that the \texttt{myFilterEvents} function filters a pattern correctly based on a boolean mask. Uses \texttt{QuickCheck} to test many input patterns and masks.
\begin{code}
-- Test: myFilterEvents (QuickCheck with NonEmpty)
prop_myFilterEvents :: NonEmptyList String -> Property
prop_myFilterEvents (NonEmpty xs) = forAll (genAlternatingMask (length xs)) $ \mask ->
  let pat      = fromList xs
      maskPat  = fromList mask
      filtered = myFilterEvents pat maskPat
      result   = map value (query filtered (stateFor (length xs)))
      expected = map fst . filter snd $ zip xs mask
  in result === expected
testMyFilterEvents :: Spec
testMyFilterEvents = describe "myFilterEvents" $
  it "filters events based on boolean mask" $
    property prop_myFilterEvents
\end{code}

\texttt{testRhythmask} tests \texttt{rhythmask} which applies a binary(1/0) string based mask. The test ensures that only "1"-marked events are retained.
\begin{code}
-- Test: rhythmask (QuickCheck)
prop_rhythmask :: NonEmptyList String -> Property
prop_rhythmask (NonEmpty xs) = forAll (genAlternatingMask (length xs)) $ \mask ->
  let maskStr = unwords (map (\b -> if b then "1" else "0") mask)
      pat     = fromList xs
      result  = map value (query (rhythmask pat maskStr) (stateFor (length xs)))
      expected = map fst . filter snd $ zip xs mask
  in result === expected
testRhythmask :: Spec
testRhythmask = describe "rhythmask" $
  it "keeps only events where mask is '1'" $
    property prop_rhythmask
\end{code}

\texttt{testRhythmaskWith} extends \texttt{rhythmask} by testing \texttt{rhythmaskWith}, which also applies a transformation (like \texttt{crush} or \texttt{gain}) to events corresponding to 1 (True). For testing purposes, we append a "!" symbol instead of musical transformations, since the testing happens completely in Haskell.
\begin{code}
-- Test: rhythmaskWith (QuickCheck)
prop_rhythmaskWith :: NonEmptyList String -> Property
prop_rhythmaskWith (NonEmpty xs) = forAll (genAlternatingMask (length xs)) $ \mask ->
  let maskStr   = unwords (map (\b -> if b then "1" else "0") mask)
      pat       = fromList xs
      transform = fmap (++ "!")
      result    = map value (query (rhythmaskWith pat maskStr transform) (stateFor (length xs)))
      kept      = map fst . filter snd $ zip xs mask
      dropped   = map ((++ "!") . fst) . filter (not . snd) $ zip xs mask
  in result === (kept ++ dropped)
testRhythmaskWith :: Spec
testRhythmaskWith = describe "rhythmaskWith" $
  it "keeps some events and transforms the rest" $
    property prop_rhythmaskWith
\end{code}

\texttt{testProbMaskPattern} validates that \texttt{probMaskPattern} produces correct boolean patterns. If the probabilities are all 0.0, no events should pass. If the probabilities are all 1.0, all events should pass.
\begin{code}
-- Test: probMaskPattern (QuickCheck)
prop_probMaskPattern_allFalse :: Positive Int -> Bool
prop_probMaskPattern_allFalse (Test.QuickCheck.Positive n) =
  let pat = probMaskPattern (replicate n 0.0)
  in all (== False) (map value $ take n $ query pat (stateFor n))
prop_probMaskPattern_allTrue :: Positive Int -> Bool
prop_probMaskPattern_allTrue (Test.QuickCheck.Positive n) =
  let pat = probMaskPattern (replicate n 1.0)
  in all (== True) (map value $ take n $ query pat (stateFor n))
testProbMaskPattern :: Spec
testProbMaskPattern = describe "probMaskPattern" $ do
  it "produces all False when probability is 0" $ property prop_probMaskPattern_allFalse
  it "produces all True when probability is 1" $ property prop_probMaskPattern_allTrue
\end{code}

\texttt{testRhythmaskProb} tests \texttt{rhythmaskProb}, which uses a probability list (like [0.81, 0.25, 1.0, 0.0]) to probabilistically include or exclude events. This test verifies that events corresponding to 0.0 probability values get filtered out.
\begin{code}
-- Test: rhythmaskProb (QuickCheck)
prop_rhythmaskProb_allFiltered :: NonEmptyList String -> Bool
prop_rhythmaskProb_allFiltered (NonEmpty xs) =
  let probs = replicate (length xs) 0.0
      pat   = fromList xs
      out   = rhythmaskProb pat probs
  in null (query out (stateFor (length xs)))
testRhythmaskProb :: Spec
testRhythmaskProb = describe "rhythmaskProb" $
  it "filters out all events when probability is 0" $ property prop_rhythmaskProb_allFiltered
\end{code}

\texttt{testRhythmaskProbWith} tests \texttt{rhythmaskProbWith}, similar to \texttt{rhythmaskWith} but for probabilistic masks. It makes sure that events with prob = 1.0 are kept and events with prob = 0.0 are transformed.
\begin{code}
-- Test: rhythmaskProbWith (QuickCheck)
prop_rhythmaskProbWith :: NonEmptyList String -> Property
prop_rhythmaskProbWith (NonEmpty xs) = forAll (genAlternatingProbs (length xs)) $ \probs ->
  let pat       = fromList xs
      transform = fmap (++ "!")
      result    = map value (query (rhythmaskProbWith pat probs transform) (stateFor (length xs)))
      kept      = map fst . filter ((== 1.0) . snd) $ zip xs probs
      dropped   = map ((++ "!") . fst) . filter ((== 0.0) . snd) $ zip xs probs
  in result === (kept ++ dropped)
testRhythmaskProbWith :: Spec
testRhythmaskProbWith = describe "rhythmaskProbWith" $
  it "keeps events with prob=1.0 and transforms others" $
    property prop_rhythmaskProbWith
\end{code}

\texttt{testRandomMaskString} checks that \texttt{randomMaskStringIO} generates a string of exactly n binary digits (1/0), useful for random but deterministic masking in a real time scenario.
\begin{code}
-- Test: randomMaskString (QuickCheck IO)
prop_randomMaskString_valid :: Positive Int -> Property
prop_randomMaskString_valid (Test.QuickCheck.Positive n) = ioProperty $ do
  s <- randomMaskStringIO n
  let bits = words s
  return $ length bits == n && all (`elem` ["0", "1"]) bits
testRandomMaskString :: Spec
testRandomMaskString = describe "randomMaskString" $
  it "generates a mask string with the specified number of bits" $
    property prop_randomMaskString_valid
\end{code}

\hide{
\begin{code}
main :: IO ()
main = hspec $ do
  testParseMask
  testMyFilterEvents
  testRhythmask
  testRhythmaskWith
  testProbMaskPattern
  testRhythmaskProb
  testRhythmaskProbWith
  testRandomMaskString
\end{code}
}

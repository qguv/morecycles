\documentclass{article}
\begin{document}

This is the test suite for the \texttt{Rhythmask} module.  
It uses \texttt{hspec} to test the following functions:
\begin{itemize}
  \item \textbf{testParseMask}: Verifies that a mask string (e.g. \texttt{"1 0 1 0"}) is parsed into exactly one cycle of boolean values.
  \item \textbf{testMyFilterEvents}: Ensures that filtering a pattern with a boolean mask returns the expected events.
  \item \textbf{testRhythmask}: Tests that applying a mask string retains only the events marked as \texttt{"1"}.
  \item \textbf{testRhythmaskWith}: Checks that the function applies a transformation to masked-out events.
  \item \textbf{testProbMaskPattern}: Verifies that a probabilistic mask produces one cycle of the expected boolean values when given all zeros or all ones.
  \item \textbf{testRhythmaskProb}: Ensures that a probabilistic mask with zero probability filters out all events.
  \item \textbf{testRhythmaskProbWith}: Checks that the transformation is applied correctly for events with probability 0, with kept events output first.
  \item \textbf{testRandomMaskString}: Confirms that the generated mask string contains the specified number of bits.
\end{itemize}

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Rhythmask 
import Sound.Tidal.Context

----------------------------------------------------------------
-- Helpers

-- | Create a State covering one cycle for n events.
stateFor :: Int -> State
stateFor n = State (Arc 0 (fromIntegral n)) 0

-- | A sample test pattern of 4 events.
testPattern :: Pattern String
testPattern = fromList ["a", "b", "c", "d"]

----------------------------------------------------------------
-- Test: parseMask
testParseMask :: Spec
testParseMask = describe "parseMask" $ do
  it "parses '1 0 1 0' into [True, False, True, False] (one cycle)" $ do
    let maskPat = parseMask "1 0 1 0"
        -- Using take to capture only one cycle.
        events  = take 4 (query maskPat (stateFor 4))
        values  = map value events
    values `shouldBe` [True, False, True, False]

----------------------------------------------------------------
-- Test: myFilterEvents
testMyFilterEvents :: Spec
testMyFilterEvents = describe "myFilterEvents" $ do
  it "filters a pattern using a boolean mask" $ do
    let maskPat  = parseMask "1 0 1 0"  -- keep events (positions) 1 and 3
        filtered = myFilterEvents testPattern maskPat
        events   = take 4 (query filtered (stateFor 4))
        values   = map value events
    values `shouldBe` ["a", "c"]

----------------------------------------------------------------
-- Test: rhythmask
testRhythmask :: Spec
testRhythmask = describe "rhythmask" $ do
  it "applies a mask string to keep only events with mask '1' (1 - True, 0 - False)" $ do
    let result = rhythmask testPattern "0 1 0 1"  -- keep events (positions) 2 and 4
        events = take 4 (query result (stateFor 4))
        values = map value events
    values `shouldBe` ["b", "d"]

----------------------------------------------------------------
-- Test: rhythmaskWith
testRhythmaskWith :: Spec
testRhythmaskWith = describe "rhythmaskWith" $ do
  it "applies a transformation to masked-out events and outputs kept events first" $ do
    -- With mask "1 0 1 0":
    --   Kept events: events (positions) 1 and 3 ("a" and "c")
    --   Transformed events: events (positions) 2 and 4 ("b!" and "d!")
    let transform = fmap (\x -> x ++ "!")
        result    = rhythmaskWith testPattern "1 0 1 0" transform
        events    = take 4 (query result (stateFor 4))
        values    = map value events
    values `shouldBe` ["a", "c", "b!", "d!"]

----------------------------------------------------------------
-- Test: probMaskPattern
testProbMaskPattern :: Spec
testProbMaskPattern = describe "probMaskPattern" $ do
  it "produces all False when probability is 0" $ do
    let pat    = probMaskPattern [0.0, 0.0, 0.0]
        events = take 3 (query pat (stateFor 3))
        values = map value events
    values `shouldBe` [False, False, False]
  it "produces all True when probability is 1" $ do
    let pat    = probMaskPattern [1.0, 1.0, 1.0]
        events = take 3 (query pat (stateFor 3))
        values = map value events
    values `shouldBe` [True, True, True]

----------------------------------------------------------------
-- Test: rhythmaskProb
testRhythmaskProb :: Spec
testRhythmaskProb = describe "rhythmaskProb" $ do
  it "filters out all events when probability is 0" $ do
    let pat    = fromList ["a", "b", "c"]
        result = rhythmaskProb pat [0.0, 0.0, 0.0]
        events = take 3 (query result (stateFor 3))
        values = map value events
    values `shouldBe` []

----------------------------------------------------------------
-- Test: rhythmaskProbWith
testRhythmaskProbWith :: Spec
testRhythmaskProbWith = describe "rhythmaskProbWith" $ do
  it "applies transformation to events with probability 0 and outputs kept events first" $ do
    -- For probabilities [1.0, 0.0, 1.0]:
    --   Kept events: events (positions) 1 and 3 ("a" and "c")
    --   Transformed event: event (position) 2 ("b!")
    let pat       = fromList ["a", "b", "c"]
        transform = fmap (\x -> x ++ "!")
        result    = rhythmaskProbWith pat [1.0, 0.0, 1.0] transform
        events    = take 3 (query result (stateFor 3))
        values    = map value events
    values `shouldBe` ["a", "c", "b!"]

----------------------------------------------------------------
-- Test: randomMaskString
testRandomMaskString :: Spec
testRandomMaskString = describe "randomMaskString" $ do
  it "generates a mask string with the specified number of bits" $ do
    let n    = 5
        s    = randomMaskString n
        bits = words s
    length bits `shouldBe` n
    all (\b -> b == "0" || b == "1") bits `shouldBe` True

----------------------------------------------------------------
-- Main: Run all tests
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
\subsubsection{Tests}

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
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Monad (replicateM)
import System.Random (randomRIO)
import RhythMask
import Sound.Tidal.Context

------------------------------------------------------------
-- Helpers

stateFor :: Int -> State
stateFor n = State (Arc 0 (fromIntegral n)) 0

randomMaskStringIO :: Int -> IO String
randomMaskStringIO n = unwords <$> replicateM n (randomRIO (0 :: Int, 1) >>= \b -> return (show b))

-- Generator: list of length n with alternating True/False
genAlternatingMask :: Int -> Gen [Bool]
genAlternatingMask n = return $ take n (cycle [True, False])

-- Generator: list of length n with alternating 1.0 and 0.0
genAlternatingProbs :: Int -> Gen [Double]
genAlternatingProbs n = return $ take n (cycle [1.0, 0.0])

------------------------------------------------------------
-- Test: parseMask (static)

testParseMask :: Spec
testParseMask = describe "parseMask" $
  it "parses '1 0 1 0' into [True, False, True, False]" $ do
    let maskPat = parseMask "1 0 1 0"
        events  = take 4 (query maskPat (stateFor 4))
        values  = map value events
    values `shouldBe` [True, False, True, False]

------------------------------------------------------------
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

------------------------------------------------------------
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

------------------------------------------------------------
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

------------------------------------------------------------
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

------------------------------------------------------------
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

------------------------------------------------------------
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

------------------------------------------------------------
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

------------------------------------------------------------
-- Main

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
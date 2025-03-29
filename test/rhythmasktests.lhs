\begin{code}
module Main where

import RhythMask
import Sound.Tidal.Pattern
import Sound.Tidal.Core
import Test.Hspec

-- A basic pattern for testing
testPattern :: Pattern String
testPattern = fromList ["a", "b", "c", "d"]

-- Debugging helpers
debugEvent :: Show a => EventF (ArcF Time) a -> String
debugEvent e =
  "Event { start = " ++ show (start (part e)) ++
  ", stop = " ++ show (stop (part e)) ++
  ", value = " ++ show (value e) ++ " }"

debugEvents :: Show a => [EventF (ArcF Time) a] -> String
debugEvents events = "[" ++ unlines (map debugEvent events) ++ "]"

-- Parse mask tests
testParseMask :: Spec
testParseMask = describe "parseMask" $ do
  it "correctly parses '1 0 1 0' into alternating True and False" $ do
    let mask = parseMask "1 0 1 0"
        state = State (Arc 0 4) 0
        values = map value (query mask state)
    values `shouldBe` [True, False, True, False]

-- myFilterEvents tests
testMyFilterEvents :: Spec
testMyFilterEvents = describe "myFilterEvents" $ do
  it "keeps only events where the mask is True" $ do
    let mask = parseMask "1 0 1 0"
        filtered = myFilterEvents testPattern mask
        state = State (Arc 0 4) 0
        resultVals = map value (query filtered state)
    resultVals `shouldBe` ["a", "c"]

-- rhythmask tests
testRhythmask :: Spec
testRhythmask = describe "rhythmask" $ do
  it "filters pattern using string mask correctly" $ do
    let result = rhythmask testPattern "1 1 0 0"
        state = State (Arc 0 4) 0
        vals = map value (query result state)
    vals `shouldBe` ["a", "b"]

-- rhythmaskWith tests
testRhythmaskWith :: Spec
testRhythmaskWith = describe "rhythmaskWith" $ do
  it "applies transformation to masked-out events" $ do
    let result = rhythmaskWith testPattern "1 0 1 0" (fast 2)
        state = State (Arc 0 4) 0
        vals = map value (query result state)
    vals `shouldSatisfy` \xs -> "a" `elem` xs && "c" `elem` xs

-- probMaskPattern + rhythmaskProb tests
testRhythmaskProb :: Spec
testRhythmaskProb = describe "rhythmaskProb" $ do
  it "retains roughly correct proportion of events probabilistically" $ do
    let pat = testPattern
        probs = replicate 4 0.0
        result = rhythmaskProb pat probs
        state = State (Arc 0 4) 0
        vals = map value (query result state)
    vals `shouldBe` []  -- With all 0.0 probs, all should be masked out

-- rhythmaskProbWith test
testRhythmaskProbWith :: Spec
testRhythmaskProbWith = describe "rhythmaskProbWith" $ do
  it "applies transformation to dropped events using probabilities" $ do
    let pat = testPattern
        probs = [1.0, 0.0, 1.0, 0.0]  -- keep 1st and 3rd
        result = rhythmaskProbWith pat probs (slow 2)
        state = State (Arc 0 4) 0
        vals = map value (query result state)
    vals `shouldSatisfy` \xs -> "a" `elem` xs && "c" `elem` xs

main :: IO ()
main = hspec $ do
  testParseMask
  testMyFilterEvents
  testRhythmask
  testRhythmaskWith
  testRhythmaskProb
  testRhythmaskProbWith
\end{code}
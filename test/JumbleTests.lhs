{-# OPTIONS_GHC -Wno-orphans #-}
\subsubsection{Tests}

\begin{code}
module Main where

import Jumble
import TestUtils

import GraceNotes

import Data.List

import Sound.Tidal.Pattern
import Sound.Tidal.Core
import Sound.Tidal.ParseBP
import Sound.Tidal.UI

import Test.Hspec
import Test.QuickCheck
\end{code}

We can now define our tests:

\begin{code}

main :: IO ()
main = hspec $ do
  -- describe "Jumble" $ do
    -- it "should change the pattern if nothing is masked" $
    --   property $ \a -> compareP a (jumble' 1 (parseBP_E "[0]") (parseBP_E "[a b]")) (parseBP_E "[b a]" :: Pattern String)

    -- it "should change the pattern with a complex mask 1" $
    --   property $ \a -> compareP a (jumble' 1 (parseBP_E "[1 0 1 0]") (parseBP_E "[a b c d]")) (parseBP_E "[a d c b]" :: Pattern String)

    -- it "should change the pattern with a complex mask 2" $
    --   property $ \a -> compareP a (jumble' 1 (parseBP_E "[1 0]") (parseBP_E "[a b c d]")) (parseBP_E "[a b d c]" :: Pattern String)

    -- it "should change the pattern with a complex mask 3" $
    --   property $ \a -> compareP a (jumble' 1 (parseBP_E "[0 [1 0]]") (parseBP_E "[a b c d]")) (parseBP_E "[b d c a]" :: Pattern String)

    -- it "should change the pattern with a complex mask 4" $
    --   property $ \a -> compareP a (jumble' 1 (parseBP_E "[1 0 1 0]") (parseBP_E "[bd [hh cp] sd cp]")) (parseBP_E "[bd [cp cp] sd hh]" :: Pattern String)

    -- it "shouldn't change the pattern when the permutation index is zero" $
    --   property $ \a mp p -> compareP a (jumble' 0 mp p) (p :: Pattern Int)

    -- it "shouldn't change the pattern when the whole pattern is masked" $
    --   property $ \a i p -> compareP a (jumble' i (parseBP_E "[1]") p) (p :: Pattern Int)

    -- it "shouldn't change the pattern when the permutation index loops around" $
    --   property $ \a -> compareP a (jumble' 2 (listToPat [True, False, True, False]) (listToPat [1, 2, 3, 4])) (listToPat [1, 2, 3, 4 :: Int])

  describe "GraceNotes" $ do

    -- it "shouldn't change the pattern when the mask is zero" $
    --   property $ \a -> compareP a (gracenotes 0 (parseBP_E "[0]") (parseBP_E "[a b c d]")) (parseBP_E "[a b c d]" :: Pattern String)


    it "shouldn't change the pattern when the mask is all zeros" $
      property $ \a -> compareP a 
        (gracenotes' 0.125 (parseBP_E "[0 0 0 0]") (parseBP_E "[a b c d]")) 
        (parseBP_E "[a b c d]" :: Pattern String)

    
    it "shouldn't make a difference what the gracenote length is when the mask is all zeroes" $
      property $ \a -> compareP a 
        (gracenotes' 0.25 (parseBP_E "[0 0 0 0]") (parseBP_E "[a b c d]" :: Pattern String)) 
        (gracenotes' 0.5 (parseBP_E "[0 0 0 0]") (parseBP_E "[a b c d]")) 

    it "should add grace notes for all events when the mask is all ones and the length is 1/8" $
      property $ \a -> counterexample
        ("Actual (floating-point):\n" ++ printPattern a (gracenotes' 0.125 (parseBP_E "[1 1 1 1]") (parseBP_E "[a b c d]") :: Pattern String) ++
        "\nExpected (floating-point):\n" ++ printPattern a correctPatternTest3)
        (compareP a 
          (gracenotes' 0.125 (parseBP_E "[1 1 1 1]") (parseBP_E "[a b c d]"))
          correctPatternTest3)

    it "should add grace notes for all events that overlap when the length is 1/4" $
      property $ \a -> counterexample
        ("Actual (floating-point):\n" ++ printPattern a (gracenotes' 0.25 (parseBP_E "[1 1 1 1]") (parseBP_E "[a b c d]") :: Pattern String) ++
        "\nExpected (floating-point):\n" ++ printPattern a correctPatternTest4)
        (compareP a 
          (gracenotes' 0.25 (parseBP_E "[1 1 1 1]") (parseBP_E "[a b c d]"))
          correctPatternTest4)

    it "should selectively add grace notes if the mask is not uniformly ones" $
      property $ \a m -> 
      let
        mask = take 4 (m ++ repeat False) :: [Bool]
        maskPattern = listToPat mask :: Pattern Bool
      in counterexample
        ("Actual (floating-point):\n" ++ printPattern a (gracenotes' 0.25 (parseBP_E "[1 1 1 1]") (parseBP_E "[a b c d]") :: Pattern String) ++
        "\nExpected (floating-point):\n" ++ printPattern a (correctPatternTest5 mask))
        (compareP a 
          (gracenotes' 0.125 maskPattern (parseBP_E "[a b c d]"))
          (correctPatternTest5 mask))

    where
      -- Create a pattern with explicit events having the exact timing we want
      createGraceNote (s, e, v) = 
        Event { 
          part = Arc s e, 
          whole = Just (Arc s e), 
          value = v 
      }
      
      correctPatternTest3 = Pattern { query = \st -> 
        let 
          -- Original pattern events
          mainEvents = query (parseBP_E "[a b c d]" :: Pattern String) st
          
          -- Determine which cycles are being queried
          arcStart = start (arc st)
          arcEnd = stop (arc st)
          startCycle = floor arcStart
          endCycle = ceiling arcEnd - 1
          
          -- Generate grace notes for all relevant cycles
          generateGraceNotesForCycle cycle =
            [
              createGraceNote ((-0.125) + fromIntegral cycle, 0 + fromIntegral cycle, "b"),
              createGraceNote (0.125 + fromIntegral cycle, 0.25 + fromIntegral cycle, "c"),
              createGraceNote (0.375 + fromIntegral cycle, 0.5 + fromIntegral cycle, "d"),
              createGraceNote (0.625 + fromIntegral cycle, 0.75 + fromIntegral cycle, "a")
            ]
          
          -- Generate grace notes for all cycles in the query range
          allGraceNotes = concatMap generateGraceNotesForCycle [startCycle..endCycle]
          
          -- Filter grace notes to only include those in the query range
          relevantGraceNotes = filter (\e -> 
            stop (part e) < arcEnd) allGraceNotes
        in
          mainEvents ++ relevantGraceNotes
      }

      correctPatternTest4 = Pattern { query = \st -> 
        let 
          -- Original pattern events
          mainEvents = query (parseBP_E "[a b c d]" :: Pattern String) st
          
          -- Determine which cycles are being queried
          arcStart = start (arc st)
          arcEnd = stop (arc st)
          startCycle = floor arcStart
          endCycle = ceiling arcEnd - 1
          
          -- Generate grace notes for all relevant cycles
          generateGraceNotesForCycle cycle =
            [
              createGraceNote ((-0.25) + fromIntegral cycle, 0 + fromIntegral cycle, "b"),
              createGraceNote (0.0 + fromIntegral cycle, 0.25 + fromIntegral cycle, "c"),
              createGraceNote (0.25 + fromIntegral cycle, 0.5 + fromIntegral cycle, "d"),
              createGraceNote (0.5 + fromIntegral cycle, 0.75 + fromIntegral cycle, "a")
            ]
          
          -- Generate grace notes for all cycles in the query range
          allGraceNotes = concatMap generateGraceNotesForCycle [startCycle..endCycle]
          
          -- Filter grace notes to only include those in the query range
          relevantGraceNotes = filter (\e -> 
            stop (part e) < arcEnd) allGraceNotes
        in
          mainEvents ++ relevantGraceNotes
      }

      correctPatternTest5 mask = Pattern { query = \st -> 
        let 
          -- Original pattern events
          mainEvents = query (parseBP_E "[a b c d]" :: Pattern String) st
          
          -- Determine which cycles are being queried
          arcStart = start (arc st)
          arcEnd = stop (arc st)
          startCycle = floor arcStart
          endCycle = ceiling arcEnd - 1
          
          -- Generate grace notes for all relevant cycles
          generateGraceNotesForCycle cycle =
            concatMap (\(m, (s, e, v)) -> 
              if m then [createGraceNote (s + fromIntegral cycle, e + fromIntegral cycle, v)] else []) 
            (zip mask 
              [((-0.125), 0, "b"), 
               (0.125, 0.25, "c"), 
               (0.375, 0.5, "d"), 
               (0.625, 0.75, "a")])
          
          -- Generate grace notes for all cycles in the query range
          allGraceNotes = concatMap generateGraceNotesForCycle [startCycle..endCycle]
          
          -- Filter grace notes to only include those in the query range
          relevantGraceNotes = filter (\e -> 
            stop (part e) < arcEnd) allGraceNotes
        in
          mainEvents ++ relevantGraceNotes
      }

printPattern :: (Show a) => Arc -> Pattern a -> String
printPattern arc pat = unlines $ map showEvent $ queryArc pat arc
  where
    showEvent (Event {part = Arc s e, value = v}) =
      "(" ++ show (realToFrac s :: Double) ++ ">" ++ show (realToFrac e :: Double) ++ ")|" ++ show v

-- Helper function to shift a pattern to a different time range
shiftPattern :: Time -> Time -> Pattern a -> Pattern a
shiftPattern start end pat = 
  compressArc (Arc start end) pat

\end{code}

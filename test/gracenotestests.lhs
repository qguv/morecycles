\subsubsection{Tests}

\begin{code}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import TestUtils

import GraceNotes

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
  describe "GraceNotes" $ do

\end{code}

The first test checks that the function doesn't change the pattern when the mask is all zeros.
When the mask is all zeros, the function should return the original pattern since no grace notes are added.

\begin{code}

    it "shouldn't change the pattern when the mask is all zeros" $
      property $ \a -> compareP a 
        (gracenotes' 0.125 (parseBP_E "[0 0 0 0]") (parseBP_E "[a b c d]")) 
        (parseBP_E "[a b c d]" :: Pattern String)

\end{code}

The second test checks that the function doesn't make a difference what the gracenote length is when the mask is all zeroes.
When the mask is all zeroes, the function should return the original pattern since no grace notes are added.

\begin{code}
    
    it "shouldn't make a difference what the gracenote length is when the mask is all zeroes" $
      property $ \a -> compareP a 
        (gracenotes' 0.25 (parseBP_E "[0 0 0 0]") (parseBP_E "[a b c d]" :: Pattern String)) 
        (gracenotes' 0.5 (parseBP_E "[0 0 0 0]") (parseBP_E "[a b c d]")) 

\end{code}

The third test checks that the function adds grace notes for all events when the mask is all ones..
When the mask is all ones, the function should add grace notes for all events in the pattern.
This test tests them for a length of 1/8.

\begin{code}

    it "should add grace notes for all events when the mask is all ones and the length is 1/8" $
      property $ \a -> counterexample
        ("Actual (floating-point):\n" ++ printPattern a (gracenotes' 0.125 (parseBP_E "[1 1 1 1]") (parseBP_E "[a b c d]") :: Pattern String) ++
        "\nExpected (floating-point):\n" ++ printPattern a correctPatternTest3)
        (compareP a 
          (gracenotes' 0.125 (parseBP_E "[1 1 1 1]") (parseBP_E "[a b c d]"))
          correctPatternTest3)

\end{code}

The fourth test checks that the function adds grace notes for all events that overlap when the length is 1/4.
This test tests them for a length of 1/4.

\begin{code}

    it "should add grace notes for all events that overlap when the length is 1/4" $
      property $ \a -> counterexample
        ("Actual (floating-point):\n" ++ printPattern a (gracenotes' 0.25 (parseBP_E "[1 1 1 1]") (parseBP_E "[a b c d]") :: Pattern String) ++
        "\nExpected (floating-point):\n" ++ printPattern a correctPatternTest4)
        (compareP a 
          (gracenotes' 0.25 (parseBP_E "[1 1 1 1]") (parseBP_E "[a b c d]"))
          correctPatternTest4)

\end{code}

The fifth test checks the functionality of the mask.
It creates random masks and tests if the grace notes are added correctly.

\begin{code}

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

\end{code}

\hide{
\begin{code}
    where
      -- Create a pattern with explicit events having the exact timing we want
      createGraceNote (s, e, v) = 
        Event { 
          part = Arc s e, 
          whole = Just (Arc s e), 
          value = v,
          Sound.Tidal.Pattern.context = Context []
      }
      
      correctPatternTest3 = Pattern { query = \st -> 
        let 
          -- Original pattern events
          mainEvents = query (parseBP_E "[a b c d]" :: Pattern String) st
          
          -- Determine which cycles are being queried
          arcStart = start (arc st)
          arcEnd = stop (arc st)
          startCycle = floor arcStart
          endCycle = ceiling arcEnd - 1 :: Int
          
          -- Generate grace notes for all relevant cycles
          generateGraceNotesForCycle c =
            [
              createGraceNote ((-0.125) + fromIntegral c, 0 + fromIntegral c, "b"),
              createGraceNote (0.125 + fromIntegral c, 0.25 + fromIntegral c, "c"),
              createGraceNote (0.375 + fromIntegral c, 0.5 + fromIntegral c, "d"),
              createGraceNote (0.625 + fromIntegral c, 0.75 + fromIntegral c, "a")
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
          endCycle = ceiling arcEnd - 1 :: Int
          
          -- Generate grace notes for all relevant cycles
          generateGraceNotesForCycle c =
            [
              createGraceNote ((-0.25) + fromIntegral c, 0 + fromIntegral c, "b"),
              createGraceNote (0.0 + fromIntegral c, 0.25 + fromIntegral c, "c"),
              createGraceNote (0.25 + fromIntegral c, 0.5 + fromIntegral c, "d"),
              createGraceNote (0.5 + fromIntegral c, 0.75 + fromIntegral c, "a")
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
          endCycle = ceiling arcEnd - 1 :: Int
          
          -- Generate grace notes for all relevant cycles
          generateGraceNotesForCycle c =
            concatMap (\(m, (s, e, v)) -> 
              if m then [createGraceNote (s + fromIntegral c, e + fromIntegral c, v)] else []) 
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

\end{code}
}

A helper function to print the patterns:
\begin{code}

printPattern :: (Show a) => Arc -> Pattern a -> String
printPattern arcRange pat = unlines $ map showEvent $ queryArc pat arcRange
  where
    showEvent (Event {part = Arc s e, value = v}) =
      "(" ++ show (realToFrac s :: Double) ++ ">" ++ show (realToFrac e :: Double) ++ ")|" ++ show v

\end{code}

The correct patterns are excluded from the report because they are too long.

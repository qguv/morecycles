 \subsubsection{Tests}

\hide{
\begin{code}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import SwingTime

import TestUtils

import Sound.Tidal.Pattern
import Sound.Tidal.Core
import Sound.Tidal.ParseBP

import Test.Hspec
import Test.QuickCheck
\end{code}
}

\hide{
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
}

We can now define our tests:

\begin{code}

main :: IO ()
main = hspec $ do
  describe "SwingTime" $ do

\end{code}
The first test checks that the function doesn't change the pattern when the mask is all zeros.

\begin{code}

    it "shouldn't change the pattern when the mask is all zeros" $
      property $ \a -> compareP a 
        (swingtime 0.125 (parseBP_E "[0 0 0 0]") (parseBP_E "[a b c d]")) 
        (parseBP_E "[a b c d]" :: Pattern String)

\end{code}

The second test checks that the function doesn't make a difference what the swing amount is when the mask is all zeroes.

\begin{code}

    it "shouldn't make a difference what the swing amount is when the mask is all zeroes" $
      property $ \a -> compareP a 
        (swingtime 0.125 (parseBP_E "[0 0 0 0]") (parseBP_E "[a b c d]" :: Pattern String)) 
        (swingtime 0.25 (parseBP_E "[0 0 0 0]") (parseBP_E "[a b c d]" :: Pattern String))

\end{code}

The third test checks that the function swings only those notes allowed by the mask.
This is done by comparing the result of the function with the result of a strict query on the pre-defined expected pattern.

\begin{code}

    it "should swing only those notes allowed by the mask" $
      property $ \a -> counterexample 
      ("Actual (floating-point):\n" ++ printPattern a (swingtime 0.125 (parseBP_E "[1 0 1 0]") (parseBP_E "[a b c d]" :: Pattern String)) ++ 
        "Expected (floating-point):\n" ++ printPattern a correctPatternTest3)
      (compareP a 
        (strictQuery a (swingtime 0.125 (parseBP_E "[1 0 1 0]") (parseBP_E "[a b c d]"))) 
        correctPatternTest3)

\end{code}

\hide{
\begin{code}

    where
      createNote (s, e, v) = 
        Event { 
          part = Arc s e, 
          whole = Just (Arc s e), 
          value = v,
          Sound.Tidal.Pattern.context = Context []
      }
      
      correctPatternTest3 = Pattern { query = \st -> 
        let 
          -- Original pattern events
          mainEvents = query (parseBP_E "[~ b ~ d]" :: Pattern String) st

          -- Determine which cycles are being queried
          arcStart = start (arc st)
          arcEnd = stop (arc st)
          startCycle = floor arcStart
          endCycle = ceiling arcEnd - 1 :: Int
          
          swungNotesForCycle c =
            [
              createNote (0.125 + fromIntegral c, 0.375 + fromIntegral c, "a"),
              createNote (0.625 + fromIntegral c, 0.875 + fromIntegral c, "c")
            ]
          
          -- Generate swing notes for all cycles in the query range
          allSwungNotes = concatMap swungNotesForCycle [startCycle..endCycle]
          
          -- Filter swing notes to only include those in the query range
          relevantSwungNotes = filter (\e -> 
            start (part e) <= arcEnd && stop (part e) >= arcStart) allSwungNotes
        in
          mainEvents ++ relevantSwungNotes
      }

strictQuery :: Arc -> Pattern a -> Pattern a
strictQuery arcRange pat  = Pattern { query = \st -> 
  let 
    events = query pat st
    filteredEvents = filter (\e -> 
      start (part e) <= stop arcRange && stop (part e) >= start arcRange) events
  in
    filteredEvents
}

-- Helper function to shift a pattern to a different time range
shiftPattern :: Time -> Time -> Pattern a -> Pattern a
shiftPattern startTime endTime pat = 
  compressArc (Arc startTime endTime) pat

printPattern :: (Show a) => Arc -> Pattern a -> String
printPattern arcRange pat = unlines $ map showEvent $ queryArc pat arcRange
  where
    showEvent (Event {part = Arc s e, value = v}) =
      "(" ++ show (realToFrac s :: Double) ++ ">" ++ show (realToFrac e :: Double) ++ ")|" ++ show v


\end{code}
}

Helper functions and correct patterns are excluded from the report because they are too long.
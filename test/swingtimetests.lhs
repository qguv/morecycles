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
    
    it "should selectively swing notes if the mask is not uniformly ones" $
      property $ \a m -> 
      let
        mask = take 4 (m ++ repeat False) :: [Bool]
        maskPattern = listToPat mask :: Pattern Bool
      in compareP a 
          (strictQuery a $ swingtime 0.125 maskPattern (parseBP_E "[a b c d]"))
          (correctPatternTest3 mask)

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
      
      correctPatternTest3 mask = Pattern { query = \st -> 
        let 
          -- Determine which cycles are being queried
          arcStart = start (arc st)
          arcEnd = stop (arc st)
          startCycle = floor arcStart
          endCycle = ceiling arcEnd - 1 :: Int
          
          swungNotesForCycle c =
            concat $
              zipWith (\m (s, e, v) -> if m then [createNote (s + fromIntegral c, e + fromIntegral c, v)] else [])
              mask
              [ (0.125, 0.375, "a"),
                (0.375, 0.625, "b"),
                (0.625, 0.875, "c"),
                (0.875, 1.125, "d")
              ]

          mainPattern = ("[" ++ (concatMap (\(m,n) -> if not m then n ++ " " else "~ ") $ zipWith (\m n -> (m,n)) (mask) [ "a", "b", "c", "d" ]) ++ "]")
          mainEvents = query (parseBP_E mainPattern) st

          -- Generate grace notes for all cycles in the query range
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
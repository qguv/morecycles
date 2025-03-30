\subsection{gracenotes}

In Western music, 
grace notes are quick, ornamental notes that precede a main note, adding expressiveness and variation.
These notes do not affect the rhythm but add embellishment to a melody.

It would be nice if musicians could specify where grace notes should be added,
allowing for dynamic and varied performances. We call this function \texttt{gracenotes}.

\texttt{gracenotes} takes two patterns as input:
a content pattern (Pattern a) and a mask pattern (Pattern Bool). 
The function produces an output pattern where any event that overlaps with a `True` in the mask pattern
is duplicated with an additional grace note occurring just before it.

\begin{code}
module GraceNotes where

import Sound.Tidal.Pattern
import Sound.Tidal.UI
import Sound.Tidal.Core
import Data.List
import Test.QuickCheck (Arbitrary(..), oneof, sized, resize)


\end{code}
the function \texttt{gracenotes} takes as input a Time variable, a Pattern Bool, and an original Pattern. The output is the resulting pattern with grace notes added.
The grace notes are added to the notes that match the mask pattern.
The Double parameter specifies how early the grace note starts, relative to the main note.
So a grace note with offset 0.125 starts 1/8th of a cycle before the main note, and ends when the main note starts.
The pitch of the grace note is always the same as the next note in the original pattern.
This, will wrap around to the next note in the pattern if the end of the pattern is reached.
\begin{code}

gracenotes' :: Time -> Pattern Bool -> Pattern a -> Pattern a
gracenotes' offset mp p = stack [original, graceNotes] where
  original = p
  -- Get all events from the original pattern for a given state
  getOriginalEvents state = query p state
  -- Create grace notes for events that match the mask
  graceNotes = Pattern{query=newQuery}
  newQuery state =
    let 
      -- Get events that match the mask
      maskedEvents = query (mask mp p) state
      -- Get all events from the original pattern
      allEvents = getOriginalEvents state
      -- If there are no events, return empty list
      result = if null maskedEvents || null allEvents
               then []
               else
                 let
                   -- Sort all events by start time to establish sequence
                   sortedEvents = sortBy (\e1 e2 -> compare (start $ part e1) (start $ part e2)) allEvents
                   -- Create a circular list of values from the original pattern
                   originalValues = cycle $ map value sortedEvents
                   -- For each masked event, find its position in the original sequence
                   -- and get the next value from the original sequence
                   createGraceNotes e =
                     let
                       eTime = start $ part e
                       -- Find the index of this event in the sorted sequence
                       -- (or the closest one if exact match not found)
                       findNextIndex [] _ = 0
                       findNextIndex [_] _ = 0
                       findNextIndex (x:xs) t =
                         if abs (start (part x) - t) < 0.0001
                         then 0  -- Found the event
                         else 1 + findNextIndex xs t
                       eventIndex = findNextIndex sortedEvents eTime
                       -- Get the next value in the original sequence (wrapping if needed)
                       nextValue = originalValues !! (eventIndex + 1)
                       -- Create the grace note
                       t0 = start $ part e
                      --  graceStart = if t0 - offset < 0 then 1 + (t0 - offset) else t0 - offset
                      --  graceEnd = graceStart + offset
                       graceStart = t0 - offset
                       graceEnd = t0
                       gracePart = Arc graceStart graceEnd
                       graceEvent = e {part = gracePart, whole=Just gracePart, value = nextValue}
                     in graceEvent
                 in map createGraceNotes maskedEvents
    in result

\end{code}
For the non-deterministic version, we need to generate a random Boolean pattern.
So we first define an instance of Arbitrary for Pattern.

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

Now we can define the non-deterministic version of \texttt{gracenotes}.
this function will work the same as the deterministic version, but will randomly generate a Boolean pattern to mask the grace notes.
\begin{code}

-- | A simpler version of gracenotes that automatically generates a random Boolean pattern
gracenotes :: Time -> Pattern a -> Pattern a
gracenotes offset p = gracenotes' offset randomMask p
  where
    -- Generate a very sparse random Boolean pattern
    randomMask = Pattern $ \(State arc _) -> 
      let 
        -- Create a pseudo-random sequence based on the arc
        seed = floor $ (*1000) $ realToFrac $ start arc
        -- Generate just one random value per cycle
        randomValue = sin (fromIntegral seed * 12345.6789)
        -- Only create a grace note ~5% of the time
        shouldAddGrace = randomValue < -0.9  -- This gives roughly 5% probability
        
        -- Create at most one event per cycle
        events = if shouldAddGrace
          then 
            let
              -- Place the grace note at a random position in the cycle
              position = (randomValue + 1) / 2  -- Convert from [-1,1] to [0,1]
              t = start arc + position * (stop arc - start arc)
              dur = (stop arc - start arc) / 16  -- Very short duration
            in [ Event 
                  { whole = Just (Arc t (t + dur))
                  , part = Arc t (t + dur)
                  , value = True
                  }
               ]
          else []
      in events

\end{code}

To test this functionality manually, you can use the following commands;
In the ghci terminal when purely working with patterns:

\begin{code}
-- p2e $ gracenotes' 0.125 (s2p "[1 0 1 0]" :: Pattern Bool) (s2p "[a b c d]" :: Pattern String)
-- p2e $ gracenotes 0.125 (s2p "[a b c d]" :: Pattern String)
\end{code}

When working with tidal, producing sounds:

\begin{code}
-- d1 $ gracenotes' 0.125 ("1 0 1 0" :: Pattern Bool) (n "c a f e" # sound "supermandolin")
\end{code}

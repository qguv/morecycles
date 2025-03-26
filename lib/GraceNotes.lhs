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

-- | gracenotes adds grace notes before events that match the mask pattern
-- The Double parameter specifies how early the grace note starts
gracenotes :: Double -> Pattern Bool -> Pattern a -> Pattern a
gracenotes startTime mp p = stack [original, graceNotes] where
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
                       offsetTime = realToFrac startTime :: Time
                       graceStart = max 0 (t0 - offsetTime)
                       graceEnd = t0
                       gracePart = Arc graceStart graceEnd
                       graceEvent = e {part = gracePart, value = nextValue}
                     in graceEvent
                 in map createGraceNotes maskedEvents
    in result


\end{code}
p2e $ gracenotes 1 (s2p "[1 0 1 0]" :: Pattern Bool) (s2p "[a b c d]" :: Pattern String)
d1 $ gracenotes' 0.125 ("1 0 1 0" :: Pattern Bool) (n "c a f e" # sound "supermandolin")

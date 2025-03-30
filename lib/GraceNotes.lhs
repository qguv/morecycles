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

{--
gracenotes adds grace notes before events that match the mask pattern
The Double parameter specifies how early the grace note starts
--}
gracenotes' :: Time -> Pattern Bool -> Pattern a -> Pattern a
gracenotes' offset mp p = stack [original, graceNotes] where
  original = p
  -- Create grace notes for events that match the mask
  graceNotes = Pattern{query=newQuery}
  newQuery state =
    let 
      -- Get events that match the mask
      maskedEvents = query (mask mp p) state
      
      -- Get the pattern for a single cycle - this is our reference pattern
      referenceState = State {arc = Arc 0 1, controls = controls state}
      referenceCycle = query p referenceState
      
      -- Sort reference events by start time to establish sequence
      sortedReference = sortBy (\e1 e2 -> compare (cyclePos $ start $ part e1) (cyclePos $ start $ part e2)) referenceCycle
      
      -- Create a circular list of values from the reference pattern
      referenceValues = cycle $ map value sortedReference
      
      -- For each masked event, find its position in the cycle and get the next value
      createGraceNote e =
        let
          -- Get normalized position in cycle (0-1)
          cyclePosTime = cyclePos $ start $ part e
          
          -- Find the closest event in the reference cycle
          findPosition [] _ = 0
          findPosition [_] _ = 0
          findPosition (x:xs) t =
            if abs (cyclePos (start (part x)) - t) < 0.0001
            then 0  -- Found the event
            else 1 + findPosition xs t
          
          eventIndex = findPosition sortedReference cyclePosTime
          
          -- Get next value, respecting the cycle structure
          nextValue = referenceValues !! (eventIndex + 1)
          
          -- Create the grace note
          t0 = start $ part e
          graceStart = t0 - offset
          graceEnd = t0
          gracePart = Arc graceStart graceEnd
          graceEvent = e {part = gracePart, whole=Just gracePart, value = nextValue}
        in 
          graceEvent
    in
      map createGraceNote maskedEvents


-- | A simpler version of gracenotes that automatically generates a random Boolean pattern
-- and uses 0.125 as the grace note duration.
gracenotes :: Pattern a -> Pattern a
gracenotes p = Pattern{query=newQuery} where
  -- Default grace note duration (1/8 of a cycle)
  defaultDuration = 0.125
  -- Use a non-deterministic approach similar to jumble
  newQuery state = 
    let randomMask = fastcat [pure True, pure False]
    in query (gracenotes' defaultDuration randomMask p) state

\end{code}

Test in ghci:
p2e $ gracenotes' 0.125 (s2p "[1 0 1 0]" :: Pattern Bool) (s2p "[a b c d]" :: Pattern String)
p2e $ gracenotes (s2p "[a b c d]" :: Pattern String)
Test in tidal:
d1 $ gracenotes' 0.125 ("1 0 1 0" :: Pattern Bool) (n "c a f e" # sound "supermandolin")

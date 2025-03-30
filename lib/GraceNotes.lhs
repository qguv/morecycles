\subsection{Gracenotes (Milan & Gideon)}

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

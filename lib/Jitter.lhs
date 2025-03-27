\subsection{jitter}
In live-coded music, perfect quantization can sometimes sound mechanical and rigid. 
Human musicians naturally introduce slight variations in timing, creating a sense of groove, 
swing, or expressiveness. In genres like jazz, funk, and experimental electronic music, 
these subtle shifts are an essential part of musical feel.  

It would be useful if musicians could introduce controlled randomness into their patterns, 
allowing each event to slightly vary in timing while still maintaining the overall rhythmic structure. 
This function, which we call \texttt{jitter}, enables such organic fluctuations by introducing small, 
randomized shifts to event start times.

\begin{code}
module Jitter where

import Sound.Tidal.Pattern
import System.Random
import System.IO.Unsafe (unsafePerformIO) -- Import unsafePerformIO to extract a random value from an IO action.
--import Sound.Tidal.Context (TidalParseError(code), begin)
\end{code}

The function \texttt{myModifyTime} enables precise timing modifications by applying a transformation function 
to the start time of each event in a pattern. This allows for controlled alterations in rhythmic feel, 
making patterns more flexible and human-like.

\begin{code}

myModifyTime :: Pattern a -> (Double -> Double) -> Pattern a
myModifyTime pat f = Pattern $ \s ->
  map updateEvent (query pat s)
  where
    -- updateEvent takes an event e and modifies its 'part' field.
    updateEvent e =
      let eventArc = part e         -- eventArc :: ArcF Time, where Time is Rational
          currentStart = start eventArc -- get the start time (a Rational)
          -- Convert the start time to Double, apply f, and convert back to Rational.
          newStart = toRational (f (realToFrac currentStart))
          -- Build a new arc with the updated start, keeping the original stop.
          newArc = eventArc { start = newStart }
      in e { part = newArc }

\end{code}

The function \texttt{jitterWith} introduces controlled timing variations by applying an offset function 
to the start time of each event in a pattern. This allows for systematic deviations from strict timing, 
enabling effects like swing, groove, or subtle fluctuations in rhythm.

\begin{code}

jitterWith :: (Double -> Double) -> Pattern a -> Pattern a --takes an offset function and a pattern as input.
jitterWith offsetFunc pat =
  myModifyTime pat (\t -> t + offsetFunc t) --returns a new pattern with the time values modified by the offset function.

\end{code}

The function \texttt{jitter} introduces natural-sounding randomness by applying a small, 
unpredictable time shift to the start of each event in a pattern. By varying event timing within 
a controlled range, it helps create a more dynamic, human-like feel in rhythmic sequences.

\begin{code}

jitter :: Pattern a -> Double -> Pattern a
jitter pat maxJitter =
  jitterWith randomOffset pat
  where
    -- randomOffset ignores its input and returns a random Double between -maxJitter and maxJitter.
    randomOffset :: Double -> Double
    randomOffset _ = unsafePerformIO (randomRIO (-maxJitter, maxJitter))

\end{code}


JitterP is a function that introduces random timing variations to a pattern, where the maximum jitter 
for each event is determined by a corresponding value in a separate pattern. Unlike \texttt{jitter}, 
which applies a fixed maximum jitter, \texttt{jitterP} dynamically determines the jitter amount based on the 
\texttt{maxJitterPat} pattern. For each event in the input pattern, it finds the corresponding event in \texttt{maxJitterPat} 
(based on overlapping time cycles) and uses its value as the range for the random shift. This allows for more 
expressive and dynamic timing variations in TidalCycles patterns.

\begin{code}
jitterP :: Pattern a -> Pattern Double -> Pattern a
jitterP pat maxJitterPat = Pattern $ \s ->
    let contentEvents = query pat s
        maxEvents     = query maxJitterPat s
        getMaxForEvent e =
            let t = start (part e)  -- current event start time (Rational)
                matching = filter (\e' ->
                        let p = part e'
                            pStart = start p
                            pStop  = stop p
                        in t >= pStart && t < pStop)
                    maxEvents
            in case matching of
                (m:_) -> value m
                []    -> 0
        updateEvent e =
            let currentStart = start (part e)
                m   = getMaxForEvent e
                offset       = unsafePerformIO (randomRIO (-m, m))
                newStart     = toRational (realToFrac currentStart + offset)
                newArc       = (part e) { start = newStart }
            in e { part = newArc }
    in map updateEvent contentEvents
\end{code}

Load this module in your TidalCycles session:
  
\texttt{:set -i"/Users/debduttaguharoy/Developer/Y2P4 - Functional Programming/Project/morecycles/lib"
:m + JitterCombined}
  
Apply jitter to a pattern with a fixed maximum jitter:
  
\texttt{d1 \$ jitter (sound "bd sn cp hh") 0.02}
  
Apply jitter to a pattern with a varying maximum jitter:
  
\texttt{d1 \$ jitterP (sound "bd sn cp hh") (range 0.01 0.05 sine)}

This will randomly shift each event's start time by up to ±0.02 cycles.
\section*{jitter}
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
import Sound.Tidal.Context -- Import rand from Sound.Tidal.Context
import System.Random
import System.IO.Unsafe (unsafePerformIO) -- Import unsafePerformIO to extract a random value from an IO action.

\end{code}

The function \texttt{myModifyTime} enables precise timing modifications by applying a transformation function to the start 
time of each event in a pattern. It works by querying all events within a given time span and then updating each event's timing arc. 
Specifically, it:

\begin{itemize}
    \item Retrieves the event's start time (stored as a Rational) from its timing arc.
    \item Converts the start time to a Double so that the transformation function can operate on it.
    \item Applies the provided function to compute a new start time.
    \item Converts the modified time back to Rational and updates the event's arc—while preserving the original stop time.
\end{itemize}

This mechanism allows for controlled alterations in rhythmic feel, making patterns more flexible and human-like. 
It serves as a core building block for higher-level functions, such as jitter effects, which introduce randomness or 
other time-based modifications into a performance.

\begin{code}

myModifyTime :: Pattern a -> (Double -> Double) -> Pattern a
myModifyTime pat f = Pattern $ \timeSpan ->
  map updateEvent (query pat timeSpan)
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

The function \texttt{jitterWith} introduces controlled timing variations by applying an offset function to the start time of 
each event in a pattern. It works by simultaneously querying two patterns: the input pattern and a continuously generated random pattern, 
using the built-in \texttt{rand} function. For each event, it takes a random value from the \texttt{rand} pattern, applies the provided offset function 
to that value, and then adds the resulting offset to the event's start time. This process creates systematic deviations from strict timing, 
enabling effects like swing, groove, or subtle rhythmic fluctuations—all while preserving the overall structure of the pattern.

Example: 
\texttt{d1 \$ jitterWith (*0.05) (sound "bd sn cp hh")}
In this example, each event's start time is shifted by an amount that is 0.05 times a random value, adding a controlled, random variation to the rhythm.


\begin{code}

jitterWith :: (Double -> Double) -> Pattern a -> Pattern a
jitterWith offsetFunc pat = Pattern $ \timeSpan ->
  let events = query pat timeSpan
      randomOffsets = query (rand :: Pattern Double) timeSpan
      updateEvent (e, r) =
        let eventArc = part e
            currentStart = start eventArc
            timeOffset = offsetFunc r -- Apply the random value to the offset function
            newStart = toRational (realToFrac currentStart + timeOffset)
            newArc = eventArc { start = newStart }
        in e { part = newArc }
  in zipWith (curry updateEvent) events (map value randomOffsets)
  
\end{code}

The function \texttt{jitter} introduces natural-sounding randomness by applying a small, unpredictable 
time shift to the start time of each event in a pattern. It creates a more dynamic, human-like feel in rhythmic sequences 
by varying event timing within a controlled range. When the provided maximum jitter is zero, the function simply returns 
the original pattern unaltered. Otherwise, it leverages \texttt{jitterWith} along with a helper function (\texttt{randomOffset}) 
that generates a random value between \(-\mathtt{maxJitter}\) and \(\mathtt{maxJitter}\). This random offset is added to each 
event's start time, ensuring that every execution produces slightly different timing variations.

\begin{code}

jitter :: Pattern a -> Double -> Pattern a
jitter pat maxJitter
  | maxJitter == 0 = pat -- No jitter when max jitter is 0
  | otherwise = jitterWith randomOffset pat
  where
    -- Generate a random offset between -maxJitter and maxJitter
    randomOffset :: Double -> Double
    randomOffset _ = unsafePerformIO (randomRIO (-maxJitter, maxJitter))

\end{code}


The function \texttt{jitterP} introduces random timing variations to a pattern, but with a twist: 
the maximum jitter applied to each event is determined dynamically by a separate pattern (\texttt{maxJitterPat}). 
For each event in the input pattern, \texttt{jitterP} identifies the corresponding event 
in \texttt{maxJitterPat}—based on overlapping time cycles—and uses its value as the upper bound for a random offset. 
A random value is then generated uniformly in the range \([-m, m]\), where \(m\) is the extracted jitter value, and 
this offset is added to the event’s start time. As a result, the rhythm is varied in a dynamic and expressive way, 
enabling more nuanced and human-like timing fluctuations while preserving the overall structure of the pattern.

\begin{code}
jitterP :: Pattern a -> Pattern Double -> Pattern a
jitterP pat maxJitterPat = Pattern $ \timespan ->
    let contentEvents = query pat timespan
        maxEvents     = query maxJitterPat timespan
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
                timeoffset       = unsafePerformIO (randomRIO (-m, m))
                newStart     = toRational (realToFrac currentStart + timeoffset)
                newArc       = (part e) { start = newStart }
            in e { part = newArc }
    in map updateEvent contentEvents
\end{code}

\subsection*{Implementation}
To test and see how \texttt{jitter} and \texttt{jitterP} work, one can follow these steps:

\begin{itemize}

  \item \textbf{Load the Module:}  
        In your TidalCycles session, ensure that your module is in the search path and load it by running:  
        \verb|:set -i"/morecycles/lib"| \\
        \verb|:m + Jitter|
        
  \item \textbf{Test the Fixed Jitter Function:}  
        Apply the \texttt{jitter} function to a pattern with a fixed maximum jitter value. For example:  
        \verb|d1 $ jitter (sound "bd sn cp hh") 0.02| \\
        This command will randomly shift the start time of each event by up to $\pm0.02$ cycles.

  \item \textbf{Test the Variable Jitter Function:}  
        Apply the \texttt{jitterP} function to a pattern using a dynamic maximum jitter value derived from a pattern. For example:  
        \verb|d1 $ jitterP (sound "bd sn cp hh") (range 0.01 0.05 sine)| \\
        Here, the maximum jitter value varies between 0.01 and 0.05 cycles following a sine wave, so the random shift applied to each event is determined by the 
        corresponding value in this pattern.

  \item \textbf{Observing the Effects:}  
        Listen carefully to the output of each command. The first test should produce a consistent, fixed range of timing variations, 
        while the second test will yield dynamically changing variations in timing, resulting in a more expressive and humanized rhythmic feel.

\end{itemize}

\subsection*{Deterministic vs. Non-Deterministic Functions}

A function is said to be \textbf{deterministic} if it always produces the same output given the same input. In other words, its behavior is completely predictable. 
For example, a function that adds 0.1 to a value, such as
\[
f(x) = x + 0.1,
\]
is deterministic because \(f(1) = 1.1\) every time it is called.

A \textbf{non-deterministic} function, on the other hand, may produce different outputs for the same input. This typically happens when the function involves 
randomness, external state, or time-dependent behavior. For example, a function that returns a random number between \(-0.02\) and \(0.02\) will produce 
different results on each call, even when provided with the same input.

In our implementation, the behavior of the functions falls into two categories:

\begin{itemize}
  \item \textbf{Deterministic Functions:}  
    A deterministic function always produces the same output given the same input and transformation. In our code, the function \texttt{myModifyTime} is deterministic. 
    It applies a user-supplied transformation function to the start time of every event in a pattern. For example, if you apply
    \begin{verbatim}
    myModifyTime pat (+0.1)
    \end{verbatim}
    every event’s start time will be shifted exactly by 0.1 cycles every time. There is no external randomness, so the output pattern is predictable.

  \item \textbf{Non-Deterministic Functions:}  
    Non-deterministic functions, on the other hand, can produce different outputs on each execution even with the same input. In our code, 
    the functions \texttt{jitterWith}, \texttt{jitter}, and \texttt{jitterP} are non-deterministic because they incorporate randomness in their calculations.

    \begin{itemize}
      \item \texttt{jitterWith} is non-deterministic when its offset function involves randomness. In our implementation, we use a function 
      that retrieves random values (from the built-in \texttt{rand} pattern or via \texttt{unsafePerformIO (randomRIO ...)}), so each time the function is 
      applied, different random offsets may be added to the event start times.

      \item \texttt{jitter} uses \texttt{jitterWith} with a helper function that generates a random offset in the range \([-\,\mathtt{maxJitter}, \mathtt{maxJitter}]\) 
      via \texttt{randomRIO}. For example, executing
      \begin{verbatim}
      d1 $ jitter (sound "bd sn cp hh") 0.02
      \end{verbatim}
      will randomly shift each event's start time by a value between -0.02 and +0.02 cycles. Even if you run the same command repeatedly, 
      the exact timing of the events will vary because of the random values.

      \item \texttt{jitterP} further extends the concept by determining the maximum jitter for each event from a provided pattern 
      (e.g., \texttt{(range 0.01 0.05 sine)}). Although the maximum allowed jitter for each event is derived from a deterministic pattern, 
      the actual offset applied is still chosen randomly within the calculated bounds. Thus, executing
      \begin{verbatim}
      d1 $ jitterP (sound "bd sn cp hh") (range 0.01 0.05 sine)
      \end{verbatim}
      yields event timings that change from cycle to cycle in a non-predictable manner.
    \end{itemize}

    \textbf{Summary Examples:}
    \begin{enumerate}
      \item With \texttt{myModifyTime}, if the transformation is \texttt{(+0.1)}, every event is delayed by exactly 0.1 cycles:
      \[
      \texttt{myModifyTime pat (+0.1)} \quad \Rightarrow \quad \text{Output is the same every time.}
      \]
      \item With \texttt{jitter}, using a fixed maximum jitter:
      \[
      \texttt{d1 \$ jitter (sound "bd sn cp hh") 0.02}
      \]
      Each execution produces different offsets for the events (each between -0.02 and +0.02 cycles), so the rhythm is varied each time.
      \item With \texttt{jitterP}, the jitter range for each event comes from a pattern. For example:
      \[
      \texttt{d1 \$ jitterP (sound "bd sn cp hh") (range 0.01 0.05 sine)}
      \]
      Here, the maximum jitter value changes according to a sine wave, and the random offset applied is within the current bound. Although 
      the \texttt{range} pattern is deterministic, the resulting event shifts are non-deterministic due to the randomness in offset selection.
    \end{enumerate}
\end{itemize}

In conclusion, while \texttt{myModifyTime} deterministically applies a specified transformation to event timings, the higher-level 
functions \texttt{jitterWith}, \texttt{jitter}, and \texttt{jitterP} incorporate randomness to produce non-deterministic, dynamic variations in timing. 
This non-determinism is essential for creating a more humanized, less mechanical rhythmic feel.
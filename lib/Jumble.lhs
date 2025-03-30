\subsection{Jumble (Quint)}

In Western music,
beats are often arranged hierarchically into a metrical structure.
Depending on the rhythm of a pattern,
some beats are more important to the pattern's structure than others which may just be present to add variety.

It would be nice if musicians could specify which beats are important,
allowing the rest to vary.
This way, a musician can specify one pattern by hand,
while many other similar patterns can be generated automatically.
We call this function \texttt{jumble}.

\hide{
\begin{code}
module Jumble where

import Sound.Tidal.Pattern
import Sound.Tidal.UI
import Sound.Tidal.Core
import Data.List
\end{code}
}

\texttt{jumble} takes two patterns as input:
a content pattern (\texttt{Pattern a})
and a mask pattern (\texttt{Pattern Bool}).
This is similar to the signature of the \texttt{mask} function.
It also takes as input an integer to select which of the shuffled patterns to generate;
i.e.\ how many times to rotate the beats which were marked as unimportant.

The function produces as output a pattern where
any event which overlaps with a 1 in the mask pattern is passed through undisturbed,
and the values at the other events are rotated.
The smallest "empty space" becomes the granularity at which the unmasked events are chopped.

\begin{code}
jumble :: Int -> Pattern Bool -> Pattern a -> Pattern a
jumble 0 _ p = p
jumble i mp p = stack [static, variable] where
  static = mask mp p
  variable = rotateValues $ mask (inv mp) p

  -- rotate a list (head to last) a certain number of times
  rot8 :: Int -> [a] -> [a]
  rot8 _ [] = []
  rot8 n' (x:xs) = if n == 0 then x:xs else rot8 (n-1) (xs ++ [x]) where
    n = n' `mod` length (x:xs)

  -- extract values from an event
  getValue :: Event a -> a
  getValue (Event _ _ _ v) = v

  -- rotate the values of an event
  rotateValues = cyclewise f where
    f events = inject (rot8 i values) events where
      values = getValue <$> events

  -- swap out the value in each event with values from a list
  inject :: [a] -> [Event b] -> [Event a]
  inject (v:vs) (e:es) = e{value=v} : inject vs es
  inject _ _ = []
\end{code}

Note that \texttt{jumble} operates on full cycles, but produces a \texttt{Pattern a}, which has the type \texttt{Arc -> [Event v]}.\footnote{\texttt{Arc} is the TidalCycles type for a timespan, and contains a start time and end time as rational numbers. The times are the number of ``cycles'' which have passed since the first line was executed. The speed of these cycles is specified by the user and is the Tidal equivalent of musical tempo.}
As the pattern produced by \texttt{jumble} has no control over which \texttt{Arc} will be given as a function parameter,
if \texttt{jumble} were to feed this input directly into the pattern it is modifying,
then it wouldn't necessarily receive \emph{all} the events which happen in the cycle;
so when rotating the unimportant beats,
there's no guarantee that the values which need to be rotated are all present.
To solve this problem, we introduce a helper function:

\begin{code}
cyclewise :: ([Event v] -> [Event v']) -> Pattern v -> Pattern v'
cyclewise f Pattern{query=oldQuery} = splitQueries Pattern{query=newQuery} where
  newQuery (State (Arc t0 t1) c) = (narrow . f) expandedEvents where
    expandedStart = fromInteger $ floor t0
    expandedEnd = fromInteger $ ceiling t1
    expandedArc = Arc expandedStart $ expandedEnd + (if expandedStart == expandedEnd then 1 else 0)
    expandedState = State expandedArc c
    expandedEvents = sortOn (\Event{part=Arc{start=t}} -> t) (oldQuery expandedState)
    narrow es = [e{part=Arc (max t0 p0) (min t1 p1)} | e@Event{part=Arc p0 p1} <- es, isIn (Arc t0 t1) (wholeStart e)]
\end{code}

As input, \texttt{cyclewise} takes a function \texttt{f}, which transforms events taking place within one cycle.
The result is a pattern manipulation function whose output pattern is smart enough to realize whenever the \texttt{Arc} given is not a full cycle,
and to expand it when necessary to capture all the events within the cycle.
The events are then fed through \texttt{f},
after which the result is contracted to fit within the \texttt{Arc} originally requested from the \texttt{Pattern}.

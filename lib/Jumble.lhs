\subsection{Jumble (Quint)}

In Western music,
beats are often arranged hierarchically into a metrical structure.
Depending on the rhythm of a pattern,
some beats are more important to the pattern's structure than others which may just be present to add variety.

It would be nice if musicians could specify which beats are important,
allowing the rest to vary automatically.
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
a content pattern (`Pattern a`)
and a mask pattern (`Pattern Bool`).
This is similar to the signature of the `mask` function.

The function produces as output a pattern where
any event which overlaps with a 1 in the mask pattern is passed through undisturbed,
and the values at the other events are rotated.
The smallest "empty space" becomes the granularity at which the unmasked events are chopped.

To implement this non-deterministic function,
we first create a deterministic version
where the permutation index is given explicitly as a parameter:

\begin{code}
-- deterministic version of jumble, which takes a permutation index
jumble' :: Int -> Pattern Bool -> Pattern a -> Pattern a
jumble' 0 _ p = p
jumble' i mp p = stack [static, variable] where
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

  cyclewise f Pattern{query=oldQuery} = splitQueries Pattern{query=newQuery} where
    newQuery (State (Arc t0 t1) c) = (narrow . f) expandedEvents where
      expandedStart = fromInteger $ floor t0
      expandedEnd = fromInteger $ ceiling t1
      expandedArc = Arc expandedStart $ expandedEnd + (if expandedStart == expandedEnd then 1 else 0)
      expandedState = State expandedArc c
      expandedEvents = sortOn (\Event{part=Arc{start=t}} -> t) (oldQuery expandedState)
      narrow es = [e{part=Arc (max t0 p0) (min t1 p1)} | e@Event{part=Arc p0 p1} <- es, isIn (Arc t0 t1) (wholeStart e)]
\end{code}

Using this definition, we can create a non-deterministic version by choosing a permutation index randomly:

\begin{code}
-- non-deterministic version of jumble, which randomly chooses a new permutation each cycle
jumble :: Pattern Bool -> Pattern a -> Pattern a
jumble _mp _p = Pattern{query=newQuery} where
  newQuery _state = undefined -- choose [jumble' i mp p | i <- [0..length (query p state)-1]] -- TODO
\end{code}

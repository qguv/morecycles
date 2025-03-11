\subsection{jumble}

In Western music,
beats are often arranged hierarchically into a metrical structure.
Depending on the rhythm of a pattern,
some beats are more important to the pattern's structure than others which may just be present to add variety.

It would be nice if musicians could specify which beats are important,
allowing the rest to vary automatically.
We call this function \texttt{jumble}.

\begin{code}
module Jumble where

import Sound.Tidal.Pattern
\end{code}

\texttt{jumble} takes two patterns as input:
a content pattern (`Pattern a`)
and a mask pattern (`Pattern Bool`).
This is similar to the signature of the `mask` function.

The function produces as output a pattern where
any event which overlaps with a 1 in the mask pattern is passed through undisturbed,
and all the other events are chopped up and shuffled.
The smallest "empty space" becomes the granularity at which the unmasked events are chopped.

To implement this non-deterministic function,
we first create a deterministic version
where the permutation index is given explicitly as a parameter:

\begin{code}
-- deterministic version of jumble, which takes a permutation index
jumble' :: Int -> Pattern Bool -> Pattern a -> Pattern a
jumble' 0 _ p = p
jumble' _n _mp _p = undefined -- FIXME
\end{code}

Using this definition, we can create a non-deterministic version by choosing a permutation index randomly:

\begin{code}
-- non-deterministic version of jumble, which randomly chooses a new permutation each cycle
jumble :: Pattern Bool -> Pattern a -> Pattern a
jumble = undefined -- FIXME
\end{code}

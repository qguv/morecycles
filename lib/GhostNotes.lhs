\subsection{ghost_notes}

In Western music,
ghost notes are subtle, muted notes that add rhythmic texture without overpowering the main melody.
These notes are often used in drumming and funk music to create groove and feel.

It would be nice if musicians could specify where ghost notes should be added,
allowing for dynamic and varied performances. We call this function \texttt{ghostnotes}.

\texttt{ghostnotes} takes two patterns as input:
a content pattern (Pattern a) and a mask pattern (Pattern Bool). 
The function produces an output pattern where any event that overlaps with a `True` in the mask pattern
is replaced with a ghost note, meaning its velocity or intensity is reduced.


\begin{code}
module GhostNotes where

import Sound.Tidal.Pattern
import Sound.Tidal.UI
import Sound.Tidal.Core
import Data.List

-- deterministic swing function with a given swing amount
ghostnotes' :: Double -> Pattern Bool -> Pattern a -> Pattern a
ghostnotes' = undefined

\end{code}
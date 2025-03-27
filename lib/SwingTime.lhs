\subsection{swing_time}
In Western music, 
particularly in jazz and blues, swing refers to a rhythmic feel where alternate beats are slightly delayed,
creating a "long-short" pattern instead of an evenly spaced beat structure. This gives the rhythm a more dynamic, groovy feel.

The \texttt{swing} function allows musicians to apply this effect by defining which beats should be shifted and by how much.
This is controlled using a mask pattern, which determines which beats remain steady and which are swung.

Deterministic Swing
Uses a fixed swing amount, consistently altering the rhythm in the same way each time.
This is useful when a specific groove is needed for synchronization.

Non-Deterministic Swing
Randomly selects a swing amount for each cycle, introducing variations in the groove.
Can create a more organic, expressive feel. Whereas the goal of \texttt{jitter} is to make the music
more human-like, the aim of non-deterministic swing is to create a dynamic groove that retains musical cohesion while introducing subtle rhythmic variation

\begin{code}
module SwingTime where

import Sound.Tidal.Pattern
import Sound.Tidal.UI
import Sound.Tidal.Core
import Data.List

-- deterministic swing function with a given swing amount
swing :: Double -> Pattern Bool -> Pattern a -> Pattern a
swing = undefined

\end{code}
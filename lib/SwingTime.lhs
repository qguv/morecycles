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
import Sound.Tidal.ParseBP
import Sound.Tidal.Context

-- deterministic swing function with a given swing amount
swing' :: Double -> Pattern Bool -> Pattern a -> Pattern a
swing' amt mp p = stack [static] where
  static = mask mp p
  swung = adjustTimes (shiftAmount amt) $ mask (inv mp) p

  shiftAmount :: Double -> Arc -> Arc
  shiftAmount amt (Arc start end) = Arc (start + shift) (end + shift)
    where 
      unit = end - start
      shift = toRational (amt * fromRational unit)

  -- Adjust times for swing feel
  adjustTimes :: (Arc -> Arc) -> Pattern a -> Pattern a
  adjustTimes f Pattern{query=oldQuery} = Pattern{query=newQuery} where
    newQuery (State a c) = [e {part = f (part e)} | e <- oldQuery (State a c)]

-- non-deterministic swing function which randomly varies the swing amount
swing :: Pattern Bool -> Pattern a -> Pattern a
swing _mp _p = Pattern{query=newQuery} where
  newQuery _state = undefined

-- let p1 = parseBP_E "[a b c d]"
-- let p2 = parseBP_E "[1 0 1 0]"

-- swing' 0.5 p2 p1

\end{code}
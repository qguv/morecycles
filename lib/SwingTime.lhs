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
swing' :: Rational -> Pattern Bool -> Pattern a -> Pattern a
swing' amt mp p = p {query = \st -> concatMap (applySwing mp st) (query p st)}
  where
    applySwing :: Pattern Bool -> State -> Event a -> [Event a]
    applySwing mask st ev = 
      case whole ev of
        Nothing -> [ev]  -- If there's no whole, return the event unchanged
        Just a -> 
          let s = start a
              e = stop a
              swingAtSt = query mask (st {arc = Arc s e})
              shouldSwing = not (null swingAtSt) && any (isTrue . value) swingAtSt
              isTrue True = True
              isTrue _ = False
              swingShift = if shouldSwing then amt else 0
              newArc = Arc (s + swingShift) (e + swingShift)
          in [ev {whole = Just newArc}]

-- let p1 = parseBP_E "[a b c d]"
-- let p2 = parseBP_E "[1 0 1 0]"

-- swing' (1/3) p2 p1
\end{code}

Test in ghci:
p2e $ swing' 0.125 (s2p "[1 0 1 0]" :: Pattern Bool) (s2p "[a b c d]" :: Pattern String)
Test in tidal:
d1 $ swing' 0.125 ("1 0 1 0" :: Pattern Bool) (n "c a f e" # sound "supermandolin")

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

-- deterministic swing function with a given swing amount
swing' :: Rational -> Pattern Bool -> Pattern a -> Pattern a
swing' amt mp p = p {query = \st -> concatMap (applySwing mp st) (query p st)}
  where
    applySwing :: Pattern Bool -> State -> Event a -> [Event a]
    applySwing maskPattern st ev = 
      case whole ev of
        Nothing -> [ev]  -- If there's no whole, return the event unchanged
        Just a -> 
          let startTime = start a
              endTime = stop a
              swingAtSt = query maskPattern (st {arc = Arc startTime endTime})
              shouldSwing = not (null swingAtSt) && any (isTrue . value) swingAtSt
              isTrue True = True
              isTrue _ = False
              swingShift = if shouldSwing then amt else 0
              newArc = Arc (startTime + swingShift) (endTime + swingShift)
          in [ev {part = newArc, whole = Just newArc}]

-- non-deterministic swing function that randomly selects a swing amount for each cycle
-- it also randomizes the mask pattern
randomswing :: Pattern a -> Pattern a
randomswing p = p {query = \st -> concatMap (applyRandomSwing st) (query p st)}
  where
    applyRandomSwing :: State -> Event a -> [Event a]
    applyRandomSwing st ev = 
      case whole ev of
        Nothing -> [ev]  -- If there's no whole, return the event unchanged
        Just a -> 
          let startTime = start a
              endTime = stop a
              -- Determine if this is an off-beat based on position in cycle
              cyclePos = startTime - (fromIntegral $ floor startTime)
              -- Simple pseudo-random function based on cycle position
              isOffBeat = (floor (cyclePos * 4) `mod` 2) == 1
              -- Generate a pseudo-random swing amount based on cycle number
              cycleNum = floor startTime
              swingAmount = 0.05 + (fromIntegral (cycleNum `mod` 11) / 100)
              -- Apply swing if it's an off-beat
              swingShift = if isOffBeat then swingAmount else 0
              newArc = Arc (startTime + swingShift) (endTime + swingShift)
          in [ev {whole = Just newArc}]

\end{code}

Test in ghci:

\begin{code}% haskell: ignore
p2e $ swing' 0.125 (s2p "[1 0 1 0]" :: Pattern Bool) (s2p "[a b c d]" :: Pattern String)
p2e $ randomswing (s2p "[a b c d]" :: Pattern String)
\end{code}

Test in tidal:

\begin{code}% haskell: ignore
d1 $ swing' 0.125 ("1 0 1 0" :: Pattern Bool) (n "c a f e" # sound "supermandolin")
\end{code}

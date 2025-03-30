\subsection{SwingTime (Milan & Gideon)}
In Western music, 
particularly in jazz and blues, swing refers to a rhythmic feel where alternate beats are slightly delayed,
creating a "long-short" pattern instead of an evenly spaced beat structure. This gives the rhythm a more dynamic, groovy feel.

The \texttt{swingtime} function allows musicians to apply this effect by defining which beats should be shifted and by how much.
This is controlled using a mask pattern, which determines which beats remain steady and which are swung.


\begin{code}
module SwingTime where

import Sound.Tidal.Pattern


\end{code}

\texttt{swingtime} takes a rational number, a mask pattern and a pattern to apply the swing to as arguments.
The rational number is the swing amount, which is the amount of time to shift the beat.
The mask pattern is a pattern of booleans that determines which beats should be shifted.
The pattern to apply the swing to is the pattern that will be changed.


\begin{code}
-- deterministic swing function with a given swing amount
swingtime :: Rational -> Pattern Bool -> Pattern a -> Pattern a
swingtime amt mp p = p {query = \st -> concatMap (applySwing mp st) (query p st)}
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
              arc = Arc (startTime + swingShift) (endTime + swingShift)
              event = if shouldSwing then ev {part = arc, whole = Just arc} else ev
              -- event = if shouldSwing then (Event arc arc (value ev) ) else ev
          in [event]

\end{code}

To test swing manually, the following commands can be used:

\begin{code}
-- p2e $ swingtime 0.125 (s2p "[1 0 1 0]" :: Pattern Bool) (s2p "[a b c d]" :: Pattern String)
\end{code}

When working with tidal, producing sounds:

\begin{code}
-- d1 $ swingtime 0.125 ("1 0 1 0" :: Pattern Bool) (n "c a f e" # sound "supermandolin")
\end{code}

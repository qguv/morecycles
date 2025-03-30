\subsection{arrhythmia}

This function takes a \texttt{ControlPattern} and a \texttt{Time} $r$ and returns
another pattern that plays normally during its first its first cycle and then 
inserts a rest of length $r$ at the beginning of the next cycle so that whatever events
occur from the time $1-r$ to 1 in the original cycle are delayed until the third cycle. This 
causes the originally patern to become progressively less in sync with its time signature
and creates interesting polyrhythms when combined with other patterns with the same signature.
 
\begin{code}

module Arrhythmia where
import Sound.Tidal.Context
import Data.Maybe
import Data.Either()

\end{code}

First we provide a function that queries a pattern during its first cycle to determine if
splitting the pattern into $n$ pieces (where $n$ is the denominator of our time input) is possible. 
Otherwise, we have events that begin in one cycle and end in another which Tidal treats as two 
separate events and plays them twice.

\begin{code}

chopper :: Integer -> Rational -> Integer -> [(Rational,Rational)]
chopper 0 _ _ = []
chopper j m k = (m,m+(1%k)) : chopper (j-1) (m+(1%k)) k

safe2Divide :: Pattern a -> Integer -> Bool
safe2Divide p k = and [and [fromJust (whole x) == eventPart x |x <- queryArc p $ uncurry Arc tpl] | tpl <- chopper k 0 k]

\end{code}

Now, a simple function that takes a pattern, some number of cycles $j$ and some cycle index $k$
and overlays $j$ cycles of silence with the pattern but only on cycles divisible by $k$.

\begin{code}

playOnly :: Int -> Int -> Pattern ValueMap -> Pattern ValueMap
playOnly j k p = every' (pure j) (pure k) (<> p) $ s $ parseBP_E "~"

\end{code}

This function queries a pattern at a specific time interval (an arc) and returns a list of 
tuples of times and patterns where the time is what portion of a cycle that pattern occupies, 
including the silences. This output can be fed into the native \texttt{timeCat} function to 
create a new pattern easily.

\begin{code}

hlpr :: Time -> [Event ValueMap] -> [(Time,ControlPattern)]
hlpr r [] = [(1-r,s $ parseBP_E "~")]
hlpr r ((Event _ _ (Arc st ft) x) : evs) = [(st-r,s $ parseBP_E "~"),(ft-st,pure x)] ++ hlpr ft evs

intervals :: ControlPattern -> Arc -> [(Time,ControlPattern)]
intervals pat ar = filter (\x -> fst x /= 0) $ hlpr (start ar) $ queryArc pat ar

\end{code}

Here, we have a function that, when given an integer $n$ and the delay time $t$, returns
the $n$th shifted cycle. For example, if shifting by one third of a cycle, the third cycle 
will begin with the last third of the original cycle, followed by one third of a cycle's rest,
followed by the first third of the original cycle.

\begin{code}

cycleCycle :: Integer -> Time -> ControlPattern -> ControlPattern
cycleCycle 0 _ pat = pat
cycleCycle j r pat = timeCat $ frontend ++ (r,s $ parseBP_E "~")
  : filter (\x -> fst x /= 0) (init backend
  ++ [((fst . last) backend - r*(j%1),(snd . last) backend)]) where
  frontend = intervals pat (Arc (1-r*((j%1)-1)) 1)
  backend  = intervals pat $ Arc 0 (1-(j%1)*r)

\end{code}

Finally, we put it all together with arrhythmia. The \texttt{<>} operator that we feed into \texttt{foldl}
plays two patterns simulaneously. So we begin with the orignal pattern, and then we fold in a pattern
that plays one of the fragmented sections only on the corresponding cycles. Again, this only requires
\texttt{denominator r} steps before we loop around to the original pattern.

\begin{code}

arrhythmia :: Time -> ControlPattern -> Either String ControlPattern
arrhythmia r pat
  | not $ safe2Divide pat (denominator r) = Left "pattern cannot be subdivided"
  | otherwise = Right $ foldl (<>) (playOnly count 0 pat) [playOnly count j (cycleCycle (toInteger j) r pat) | j <- take (count-1) [1..]] where
      count = fromIntegral (1 + denominator r)

arrhythmiaUnsafe :: Time -> ControlPattern -> ControlPattern
arrhythmiaUnsafe r pat = foldl (<>) (playOnly count 0 pat) [playOnly count j (cycleCycle (toInteger j) r pat) | j <- take (count-1) [1..]] where
      count = fromIntegral (1 + denominator r)

arcStart :: Arc -> Time
arcStart (Arc t _) = t

arcEnd :: Arc -> Time
arcEnd (Arc _ t) = t

\end{code}

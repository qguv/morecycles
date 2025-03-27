"RhythMask": Probability-Based Masking
The idea behind RhythMask is to create a rhythmic effect where some beats are 
probabilistically dropped or kept each cycle, 
rather than being strictly fixed by a binary mask.

Theoretical implementation idea
Input:
-> A content pattern (Pattern a): The original rhythmic sequence.
-> A probability pattern (Pattern Double): A probability value (between 0.0 and 1.0) 
that determines the likelihood of each beat being played.

Output: A modified version of the input pattern with beats dropped probabilistically

Steps of implementation:
->Extract Events: The function first queries the input pattern to get a list of its events.
->Apply Probability Filtering: Each event is evaluated against the corresponding probability value.
->Random Decision Making: A random number is generated for each event, and if it is below the 
probability threshold, the event passes on to the final output, otherwise it is removed.
->Reconstruction of the Pattern: The remaining beats are reconstructed into a new TidalCycles pattern.

Additional Functionality:
-> Instead of passing a set of probability values, I am now passing a binary mask (e.g. [1 0 1 0]),
where 1 is True and 0 is False. This determines which beats will be kept in a sequence and which
ones will be dropped. However, there is a helper function at the beginning, that generates a random
mask.

\begin{code}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Rhythmask where

import Sound.Tidal.Context

randomMaskString :: Int -> String
randomMaskString n = unwords $ take n $ map (\x -> if even (x * 37 `mod` 7) then "1" else "0") [1..]

{-|
  parseMask converts a binary mask string (e.g. "1 0 1 0") into a Pattern Bool.
  It splits the string into words, maps "1" to True and any other word to False,
  cycles the resulting list, and then limits it to one cycle using take.
-}
parseMask :: String -> Pattern Bool
parseMask s =
  let bits = map (== "1") (words s)
  in fastcat (map pure bits)
{-|
  myFilterEvents takes a pattern 'pat' and a boolean mask pattern 'maskPat'
  and produces a new pattern that only keeps events where the mask is True.
  (It extracts the events from both patterns at a given time, zips them, and
  keeps an event if its corresponding mask event (extracted via value) is True.)
-}
myFilterEvents :: Pattern a -> Pattern Bool -> Pattern a
myFilterEvents pat maskPat = Pattern $ \s ->
  let es = query pat s
      bs = query maskPat s
      filtered = [ e | (e, b) <- zip es bs, value b ]
  in filtered

{-|
  rhythmask applies a mask (given as a string like "1 0 1 0") to a pattern.
  Only events corresponding to a True (or "1") in the mask will be kept.
-}
rhythmask :: Pattern a -> String -> Pattern a
rhythmask pat maskStr = myFilterEvents pat (parseMask maskStr)

{-|
  rhythmaskWith applies a transformation to the events that are masked out.
  It splits the pattern into two parts:
    1. 'kept' events where the mask is True,
    2. 'transformed' events (obtained by applying the given transformation)
       where the mask is False.
  These two layers are then combined using 'stack'.
-}
rhythmaskWith :: Pattern a -> String -> (Pattern a -> Pattern a) -> Pattern a
rhythmaskWith pat maskStr transform =
  let maskP    = parseMask maskStr
      notMaskP = fmap not maskP
      kept         = myFilterEvents pat maskP
      transformed  = myFilterEvents (transform pat) notMaskP
  in stack [kept, transformed]



\end{code}
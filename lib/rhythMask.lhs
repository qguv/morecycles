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

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module RhythmMask where
import Sound.Tidal.Context
import Data.List (find)

-- | Parses a binary rhythm mask into a Pattern Bool
parseMask :: String -> Pattern Bool
parseMask s = boolPat
  where
    bits = cycle $ map (== "1") $ words s  -- Compares String == String
    boolPat = segment (fromIntegral $ length $ words s) $ slowcat $ map pure bits

-- | Applies a rhythm mask to a pattern, keeping only the "1" parts of the mask
rhythmask :: String -> Pattern a -> Pattern a
rhythmask maskStr = filterByPattern (const True) (parseMask maskStr)

rhythmaskWith :: String -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
rhythmaskWith maskStr transform pat =
  stack
    [ filterByPattern (const True) maskP pat
    , filterByPattern (const True) notMaskP (transform pat)
    ]
  where
    maskP = parseMask maskStr
    notMaskP = fmap not maskP

-- | Filters pattern values based on a Pattern Bool
filterByPattern :: (a -> Bool) -> Pattern Bool -> Pattern a -> Pattern a
filterByPattern _ cond p = Pattern $ \t ->
  let conds = map (\e -> (eventPart e, value e)) (queryArc cond (arc t))
      events = queryArc p (arc t)
      matches = \e -> case lookupEvent (eventPart e) conds of
                        Just True -> True
                        _         -> False
  in filter matches events

-- | Looks up whether an event should pass based on arc match
lookupEvent :: Arc -> [(Arc, Bool)] -> Maybe Bool
lookupEvent a = fmap snd . find (\(a', _) -> a' == a)

-- | Example usage (to try in GHCi or a `.tidal` script):
-- d1 $ rhythmask "1 0 1 1 0" $ sound "bd sn hh cp arpy"
-- d2 $ rhythmaskWith "1 0 0 1" (|+| gain 0.3) $ sound "arpy*4"


\end{code}
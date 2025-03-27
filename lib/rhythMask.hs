{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Rhythmask where

import Sound.Tidal.Context

-- | Probabilistic mask: generates a Pattern Bool from a list of Doubles (0.0 to 1.0)
probMaskPattern :: [Double] -> Pattern Bool
probMaskPattern probs =
  fastcat $ map (\p -> fmap (< p) (rand :: Pattern Double)) probs

-- | Applies a probabilistic mask to a pattern
rhythmaskProb :: Pattern a -> [Double] -> Pattern a
rhythmaskProb pat probs = myFilterEvents pat (probMaskPattern probs)

-- | Applies a transformation to masked-out events using a probabilistic mask
rhythmaskProbWith :: Pattern a -> [Double] -> (Pattern a -> Pattern a) -> Pattern a
rhythmaskProbWith pat probs transform =
  let maskP    = probMaskPattern probs
      notMaskP = fmap not maskP
      kept         = myFilterEvents pat maskP
      transformed  = myFilterEvents (transform pat) notMaskP
  in stack [kept, transformed]

-- | generates a random set of True/False (0/1) values to drop or keep beats.
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

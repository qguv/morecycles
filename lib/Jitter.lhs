\begin{code}
-- | Module: Jitter
-- This module defines a function 'jitter' that introduces small random timing
-- variations (jitters) to the start time of each event in a TidalCycles pattern.
module Jitter where

-- Import unsafePerformIO to extract a random value from an IO action.
import Sound.Tidal.Pattern
import System.Random
import System.IO.Unsafe (unsafePerformIO)

{-|
  myModifyTime :: Pattern a -> (Double -> Double) -> Pattern a

  Purpose:
    Apply a pure offset function to every event's start time (the 'start' field of the
    event's 'part' arc) in a pattern.
  
  Parameters:
    pat :: Pattern a
      The input pattern whose events will be modified.
  
    f :: Double -> Double
      A function that takes the current start time (as a Double) and returns a modified time.
  
  Returns:
    A new pattern in which each event's start time (within its arc) is updated by f.
  
  Note:
    Tidal events have a 'part' field of type 'ArcF Time' (usually an arc with a start and stop).
    Here we update only the start value of that arc.
-}
myModifyTime :: Pattern a -> (Double -> Double) -> Pattern a
myModifyTime pat f = Pattern $ \s ->
  map updateEvent (query pat s)
  where
    -- updateEvent takes an event e and modifies its 'part' field.
    updateEvent e =
      let eventArc = part e         -- eventArc :: ArcF Time, where Time is Rational
          currentStart = start eventArc -- get the start time (a Rational)
          -- Convert the start time to Double, apply f, and convert back to Rational.
          newStart = toRational (f (realToFrac currentStart))
          -- Build a new arc with the updated start, keeping the original stop.
          newArc = eventArc { start = newStart }
      in e { part = newArc }

{-|
  jitterWith :: (Double -> Double) -> Pattern a -> Pattern a

  Purpose:
    Apply a pure offset function to every event's start time in a pattern.
  
  Parameters:
    offsetFunc :: Double -> Double
      A function that takes an event's current start time (as a Double) and returns an
      additional time offset.
  
    pat :: Pattern a
      The input pattern whose events will be modified.
  
  Returns:
    A new pattern in which each event's start time has been shifted by offsetFunc.
  
  This function builds on our 'myModifyTime' helper.
-}
jitterWith :: (Double -> Double) -> Pattern a -> Pattern a
jitterWith offsetFunc pat =
  myModifyTime pat (\t -> t + offsetFunc t)

{-|
  jitter :: Pattern a -> Double -> Pattern a

  Purpose:
    Introduce random timing variations ("jitters") to each event's start time.
  
  Parameters:
    pat :: Pattern a
      The original pattern whose events will receive a random time shift.
  
    maxJitter :: Double
      The maximum absolute jitter (in cycles). Each event's start time is shifted by a random
      amount between -maxJitter and +maxJitter.
  
  Returns:
    A new pattern with each event's start time randomly adjusted.
  
  NOTE:
    This function uses 'unsafePerformIO' to generate randomness. While this breaks purity,
    it is acceptable in a live coding context.
-}
jitter :: Pattern a -> Double -> Pattern a
jitter pat maxJitter =
  jitterWith randomOffset pat
  where
    -- randomOffset ignores its input and returns a random Double between -maxJitter and maxJitter.
    randomOffset :: Double -> Double
    randomOffset _ = unsafePerformIO (randomRIO (-maxJitter, maxJitter))

{-|
  Example Usage:

  In your TidalCycles session (e.g., in Atom or VS Code), load the Jitter module:

      :m + Jitter

  Then you can apply jitter to a pattern. For example:

      d1 $ jitter (sound "bd sn cp hh") 0.02

  This will randomly shift each event's start time by up to ±0.02 cycles.
-}
\end{code}

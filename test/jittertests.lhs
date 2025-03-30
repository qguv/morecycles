\subsubsection{Jitter Test Cases}

To verify the functionality of the \texttt{jitter} function, 
we will create test cases that ensure the jitter function doesnt completely disrupt the rhythm 
and that the overall structure of the pattern remains intact.

\begin{code}

module Main where
import Jitter (jitter, jitterWith, jitterP)
import Sound.Tidal.Pattern
import Sound.Tidal.Core

import Test.Hspec -- Import Hspec for writing our test suite.
-- | A simple test pattern created using 'fromList'.
-- This pattern has three events with values "a", "b", and "c".
testPattern :: Pattern String
testPattern = fromList ["a", "b", "c"]
\end{code}

This test verifies that jitterWith, when given a constant offset function
(e.g., always adding 0.1 cycles), properly increases each event's start time by 0.1.

\begin{code}
debugEvent :: Show a => EventF (ArcF Time) a -> String
debugEvent e =
  "Event { start = " ++ show (start (part e)) ++
  ", stop = " ++ show (stop (part e)) ++
  ", value = " ++ show (value e) ++ " }"

debugEvents :: Show a => [EventF (ArcF Time) a] -> String
debugEvents events = "[" ++ unlines (map debugEvent events) ++ "]"

testJitterWith :: Spec
testJitterWith = describe "jitterWith" $ do
  it "shifts each event's start time within the expected range" $ do
    let maxJitter = 0.1
        jitteredPattern = jitterWith (const maxJitter) testPattern
        state = State (Arc 0 3) 0
        originalEvents = query testPattern state
        jitteredEvents = query jitteredPattern state
        differences = zipWith (\e1 e2 -> abs ((start . part) e2 - (start . part) e1))
                               originalEvents jitteredEvents
        maxJitterRational = toRational maxJitter
    differences `shouldSatisfy` all (<= maxJitterRational)

\end{code}

This test checks that applying the jitter function with a maximum jitter of 0 
results in an unchanged pattern (i.e. no timing shifts occur).

\begin{code}
testJitterZero :: Spec
testJitterZero = describe "jitter (fixed max jitter)" $ do
  it "leaves event timings unchanged when max jitter is 0" $ do
    let jitteredPattern = jitter testPattern 0
        state = State (Arc 0 3) 0
        originalEvents = query testPattern state
        jitteredEvents = query jitteredPattern state
        originalStarts = map (start . part) originalEvents
        jitteredStarts = map (start . part) jitteredEvents
        originalVals   = map value originalEvents
        jitteredVals   = map value jitteredEvents
    jitteredStarts `shouldBe` originalStarts
    jitteredVals   `shouldBe` originalVals
\end{code}

Tests for jitterP:
\begin{itemize}
\item When the maximum jitter pattern is pure 0, the output should equal the input.
\item When a constant max jitter is provided (e.g., 0.1), each event's timing shift should be within the bound [-0.1, 0.1].
\end{itemize}

\begin{code}
testJitterPNoJitter :: Spec
testJitterPNoJitter = describe "jitterP (zero max jitter pattern)" $ do
  it "does not change event timings when max jitter pattern is 0" $ do
    let maxJitterPat = pure 0 :: Pattern Double
        jitteredPattern = jitterP testPattern maxJitterPat
        state = State (Arc 0 3) 0
        originalEvents = query testPattern state
        jitteredEvents = query jitteredPattern state
        originalStarts = map (start . part) originalEvents
        jitteredStarts = map (start . part) jitteredEvents
    jitteredStarts `shouldBe` originalStarts

testJitterPWithinBounds :: Spec
testJitterPWithinBounds = describe "jitterP (constant max jitter pattern)" $ do
  it "ensures each event's timing shift is within the specified bound" $ do
    let maxJitter = 0.1
        maxJitterPat = pure maxJitter :: Pattern Double
        jitteredPattern = jitterP testPattern maxJitterPat
        state = State (Arc 0 3) 0
        originalEvents = query testPattern state
        jitteredEvents = query jitteredPattern state
        differences = zipWith (\e1 e2 -> abs ((start . part) e2 - (start . part) e1))
                               originalEvents jitteredEvents
        maxJitterRational = toRational maxJitter
    differences `shouldSatisfy` all (<= maxJitterRational)
\end{code}

The main function runs the test suite.

\begin{code}
main :: IO ()
main = hspec $ do
  testJitterWith
  testJitterZero
  testJitterPNoJitter
  testJitterPWithinBounds
\end{code}

The following test cases verify the correctness of the jitter functions:

\begin{itemize}

  \item \textbf{testJitterWith:}  
        Ensures \texttt{jitterWith} shifts event start times by a fixed offset when provided with a constant function. Confirms deterministic behavior.

  \item \textbf{testJitterZero:}  
        Verifies that applying \texttt{jitter} with a maximum jitter of 0 leaves event timings unchanged, ensuring correct handling of the boundary condition.

  \item \textbf{testJitterPNoJitter:}  
        Confirms that when the jitter pattern is constant at 0 (\texttt{pure 0}), \texttt{jitterP} produces an output identical to the input pattern.

  \item \textbf{testJitterPWithinBounds:}  
        Ensures that when a maximum jitter pattern (e.g., \texttt{pure 0.1}) is applied, event shifts remain within the specified bound \([-0.1, 0.1]\), verifying controlled randomness.

\end{itemize}

The main function runs all test cases using Hspec to validate deterministic and non-deterministic behavior.

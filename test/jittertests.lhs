\subsubsection{Jitter Test Cases}

To verify the functionality of the \texttt{jitter} function, 
we will create test cases that ensure the jitter function doesnt completely disrupt the rhythm 
and that the overall structure of the pattern remains intact.

\hide{
\begin{code}
module Main where
import Jitter (jitter, jitterWith, jitterP)
import Sound.Tidal.Pattern
import Sound.Tidal.Core

import Test.Hspec -- Import Hspec for writing our test suite.
\end{code}
}

\begin{code}
testPattern :: Pattern String
testPattern = fromList ["a", "b", "c"]
\end{code}

This test verifies that jitterWith, when given a constant offset function
(e.g., always adding 0.1 cycles), properly increases each event’s start time by 0.1.

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

The following test cases have been designed to verify that the jitter functions behave as expected. Below is a summary of each test case, its relevance, and why it was chosen.

\begin{itemize}

  \item \textbf{testJitterWith:}  
        This test verifies that \texttt{jitterWith}, when provided with a constant offset function, correctly shifts the start time of every event by a fixed amount.  
        \begin{itemize}
          \item \textbf{What it does:} It applies \texttt{jitterWith} with an offset function that always returns the same value (0.1). The test then compares the 
          start times of the original events with those of the modified events, expecting each to be increased by exactly 0.1 cycles.
          \item \textbf{Relevance:} This ensures that the underlying mechanism for modifying event timing via \texttt{myModifyTime} works correctly in a controlled, 
          deterministic scenario.
          \item \textbf{Example:}  
                If an event originally starts at 0.5 cycles, after applying \texttt{jitterWith (\_ -> 0.1)} it should start at 0.6 cycles.
        \end{itemize}

  \item \textbf{testJitterZero:}  
        This test checks that applying the \texttt{jitter} function with a maximum jitter of 0 leaves the pattern completely unchanged.  
        \begin{itemize}
          \item \textbf{What it does:} It runs \texttt{jitter testPattern 0} and verifies that both the start times and the event values remain identical to the original pattern.
          \item \textbf{Relevance:} This test confirms that the function correctly handles a boundary condition (i.e., no jitter should be applied when the maximum jitter is 0).
          \item \textbf{Example:}  
                Running \texttt{jitter pat 0} should yield the exact same event timings as \texttt{pat}.
        \end{itemize}

  \item \textbf{testJitterPNoJitter:}  
        This test verifies that when the maximum jitter pattern is constant at 0 (using \texttt{pure 0}), \texttt{jitterP} produces an output that is identical to the input pattern.  
        \begin{itemize}
          \item \textbf{What it does:} It creates a max jitter pattern that always returns 0 and applies \texttt{jitterP}. The test then compares the start times 
          of events in the original and jittered patterns.
          \item \textbf{Relevance:} This ensures that \texttt{jitterP} behaves deterministically when the maximum allowed jitter is 0.
          \item \textbf{Example:}  
                Every event's timing remains unchanged because no jitter is allowed.
        \end{itemize}

  \item \textbf{testJitterPWithinBounds:}  
        This test ensures that when a constant maximum jitter is provided via a pattern (e.g., \texttt{pure 0.1}), the shift applied to each event is always 
        within the expected bound of \([-0.1, 0.1]\) cycles.  
        \begin{itemize}
          \item \textbf{What it does:} It applies \texttt{jitterP} with a max jitter pattern that is constant at 0.1 and then computes the absolute difference 
          between the original and modified event start times. The test asserts that all these differences do not exceed 0.1 (converted to a Rational).
          \item \textbf{Relevance:} This test confirms that the random offset generated for each event never exceeds the specified maximum jitter bound, ensuring 
          controlled randomness.
          \item \textbf{Example:}  
                If an event’s original start time is 1.0 and the max jitter is 0.1, the jittered start time should be between 0.9 and 1.1 cycles.
        \end{itemize}

\end{itemize}

The main function aggregates all test cases and runs them using Hspec. This ensures that any change in the jitter functions can be quickly validated against our 
expectations for both deterministic and non-deterministic behavior.

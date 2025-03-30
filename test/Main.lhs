module Main where

import GraceNotesTests
import JumbleTests

main :: IO ()
main = do
  GraceNotesTests.main
  JumbleTests.main
  putStrLn "All tests passed!"
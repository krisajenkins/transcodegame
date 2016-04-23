module Main (..) where

import Console
import ElmTest exposing (..)
import Task exposing (Task)
import NarrativeTest


tests : Test
tests =
  suite
    "All"
    [ NarrativeTest.tests ]


port runner : Signal (Task x ())
port runner =
  Console.run (consoleRunner tests)

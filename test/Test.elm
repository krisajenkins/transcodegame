module Test exposing (..)

import ElmTest exposing (..)
import NarrativeTest


tests : Test
tests =
    suite "All"
        [ NarrativeTest.tests ]


{-| Run the whole test suite.
-}
main : Program Never
main =
    runSuite tests

module NarrativeTest (tests) where

import ElmTest exposing (..)
import Narrative exposing (..)
import Types exposing (..)
import State


tests : Test
tests =
  ElmTest.suite
    "Narrative"
    [ handleCommandTests
    ]


handleCommandTests : Test
handleCommandTests =
  ElmTest.suite
    "handleCommand"
    [ defaultTest
        (assertEqual
          [ Molotov ]
          (handleCommands
            [ (PickUp ( 11, 9 ) Cinzano)
            , (PickUp ( 6, 9 ) Rag)
            , (Use Rag Cinzano)
            ]
            |> .player
            |> .inventory
          )
        )
    ]


handleCommands : List Command -> Model
handleCommands =
  List.foldl
    (\command -> handleCommand command >> fst)
    State.initialModel

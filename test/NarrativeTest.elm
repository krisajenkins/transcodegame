module NarrativeTest exposing (tests)

import ElmTest exposing (..)
import Narrative exposing (..)
import State
import Types exposing (..)


tests : Test
tests =
    ElmTest.suite "Narrative"
        [ handleCommandTests
        ]


handleCommandTests : Test
handleCommandTests =
    ElmTest.suite "handleCommand"
        [ defaultTest
            (assertEqual []
                (handleCommands
                    [ (PickUp ( 11, 9 ) Cinzano)
                    , (PickUp ( 6, 9 ) Rag)
                    , (PickUp ( 14, 2 ) Lighter)
                    , (Use Rag Cinzano)
                    , (Use Lighter Molotov)
                    , (Use MolotovLit Shed)
                    ]
                    |> .player
                    |> .inventory
                )
            )
        ]


handleCommands : List Command -> Model
handleCommands =
    List.foldl (\command -> handleCommand command >> fst)
        (fst State.initialState)

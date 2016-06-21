module State exposing (..)

import Array
import Astar
import Dict
import Narrative exposing (..)
import Time exposing (Time)
import Types exposing (..)


stepTime : Time
stepTime =
    100 * Time.millisecond


wall : List Position -> List ( Position, Cell )
wall =
    List.map (\coords -> ( coords, Block ))


verticalWall : Int -> List Int -> List ( Position, Cell )
verticalWall x ys =
    ys
        |> List.map (\y -> ( x, y ))
        |> wall


horizontalWall : List Int -> Int -> List ( Position, Cell )
horizontalWall xs y =
    xs
        |> List.map (\x -> ( x, y ))
        |> wall


initialWorld : World Cell
initialWorld =
    let
        ( minX, minY ) =
            ( 0, 0 )

        ( maxX, maxY ) =
            ( 17, 10 )

        xRange =
            [minX..maxX]

        yRange =
            [minY..maxY]
    in
        (List.concat
            [ List.concatMap
                (\x ->
                    List.map (\y -> ( ( x, y ), Path ))
                        yRange
                )
                xRange
            , verticalWall minX yRange
            , verticalWall maxX yRange
            , horizontalWall xRange minY
            , horizontalWall xRange maxY
            , verticalWall 5 [1..4]
            , horizontalWall [5..8] 4
            , verticalWall 12 [1..3]
            , horizontalWall [5..8] 6
            , verticalWall 5 [6..9]
            , horizontalWall [10..12] 6
            , verticalWall 12 [5..9]
            , [ ( ( 8, 1 ), Block ) ]
            , [ ( ( 8, 3 ), Block ) ]
            , [ ( ( 16, 5 ), Thing Shed ) ]
            , [ ( ( 1, 4 ), Thing Chicken ) ]
            , [ ( ( 1, 5 ), Thing Postbox ) ]
            , [ ( ( 6, 3 ), Thing PotatoSackFull ) ]
            , [ ( ( 11, 2 ), Thing Fridge ) ]
            , [ ( ( 14, 2 ), Thing Lighter ) ]
            , [ ( ( 15, 2 ), Thing Still ) ]
            , [ ( ( 6, 9 ), Thing Rag ) ]
            , [ ( ( 7, 7 ), Thing Paperwork ) ]
            , [ ( ( 11, 9 ), Thing Cinzano ) ]
            ]
        )
            |> Dict.fromList


initialState : ( Model, Cmd Msg )
initialState =
    ( { world = initialWorld
      , dialogue = Just "Well, I guess I'd better look around..."
      , partialCommand = Nothing
      , player =
            { position = ( 1, 1 )
            , inventory = []
            }
      , destination = Nothing
      , queuedCommand = Nothing
      , timeSinceLastMove = Nothing
      }
    , Cmd.none
    )


updateWithDialogue : Msg -> Model -> ( Model, Maybe String )
updateWithDialogue action model =
    case action of
        PlayerCommand command ->
            handleCommand command model

        Tick time ->
            handleWalk time model


handleWalk : Time -> Model -> ( Model, Maybe String )
handleWalk time model =
    case model.destination of
        Nothing ->
            ( { model | timeSinceLastMove = Nothing }
            , Nothing
            )

        Just destination ->
            let
                previousTime =
                    Maybe.withDefault 0 model.timeSinceLastMove
            in
                if previousTime + stepTime > time then
                    ( model, Nothing )
                else
                    case
                        Astar.findPath estimatedDistance
                            (validMovesFrom model.world)
                            (model.player.position)
                            destination
                    of
                        Nothing ->
                            ( { model
                                | destination = Nothing
                                , timeSinceLastMove = Nothing
                              }
                            , Just "Hmm...I can't find a way there."
                            )

                        Just path ->
                            case Array.get 0 path of
                                Nothing ->
                                    let
                                        newModel =
                                            { model
                                                | destination = Nothing
                                                , timeSinceLastMove = Nothing
                                                , queuedCommand = Nothing
                                            }
                                    in
                                        case model.queuedCommand of
                                            Nothing ->
                                                ( newModel, Just "J'arrive!" )

                                            Just command ->
                                                { newModel | queuedCommand = Nothing }
                                                    |> handleCommand command

                                Just p ->
                                    let
                                        player =
                                            model.player
                                    in
                                        ( { model
                                            | player = { player | position = p }
                                            , timeSinceLastMove = Just time
                                          }
                                        , Nothing
                                        )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    let
        ( newModel, newDialogue ) =
            updateWithDialogue action model
    in
        if newDialogue == Nothing then
            ( newModel, Cmd.none )
        else
            ( { newModel | dialogue = newDialogue }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.destination == Nothing then
        Sub.none
    else
        Time.every 50 Tick

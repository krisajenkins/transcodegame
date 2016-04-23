module State (..) where

import Narrative exposing (..)
import Types exposing (..)
import Effects exposing (..)
import Dict
import Time exposing (Time)
import Astar
import Array


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
      ( 15, 7 )

    xRange =
      [minX..maxX]

    yRange =
      [minY..maxY]
  in
    (List.concat
      [ List.concatMap
          (\x ->
            List.map
              (\y -> ( ( x, y ), Path ))
              yRange
          )
          xRange
      , verticalWall minX yRange
      , verticalWall maxX yRange
      , horizontalWall xRange minY
      , horizontalWall xRange maxY
      , horizontalWall [4..10] 4
      , verticalWall 4 [4..6]
      , verticalWall 8 [2..6]
      , horizontalWall [12..13] 2
      , [ ( ( 9, 6 )
          , Thing Cinzano
          )
        ]
      ]
    )
      |> Dict.fromList


initialModel : Model
initialModel =
  { world = initialWorld
  , dialogue = Just "Well, I guess I'd better look around..."
  , hint = Nothing
  , partialCommand = Nothing
  , player =
      { position = ( 1, 1 )
      , inventory = []
      }
  , destination = Nothing
  , queuedCommand = Nothing
  , timeSinceLastMove = Nothing
  }


initialEffects : Effects Action
initialEffects =
  none


updateWithDialogue : Action -> Model -> ( Model, Maybe String )
updateWithDialogue action model =
  case action of
    Hint Nothing ->
      ( { model | hint = Nothing }
      , Nothing
      )

    Hint (Just command) ->
      ( { model | hint = handleHint command model }
      , Nothing
      )

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
            Astar.findPath
              estimatedDistance
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
                        handleCommand command newModel

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


update : Action -> Model -> Model
update action model =
  let
    ( newModel, newDialogue ) =
      updateWithDialogue action model
  in
    if newDialogue == Nothing then
      newModel
    else
      { newModel | dialogue = newDialogue }


effects : Action -> ( Model, Model ) -> Effects Action
effects action ( _, model ) =
  if model.destination == Nothing then
    none
  else
    tick Tick

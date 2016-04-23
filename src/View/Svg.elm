module View.Svg (root) where

import Common.View exposing (..)
import Signal exposing (..)
import Html exposing (Html)
import Types exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Dict


tileSize : Int
tileSize =
  40


root : Address Action -> Model -> Html
root address model =
  svg
    [ width (px 800)
    , height (px 320)
    ]
    [ g
        []
        (model.world
          |> Dict.toList
          |> List.map (uncurry (tile address))
        )
    , tile address model.player.position Character
    ]


tile : Address Action -> Position -> Cell -> Svg
tile address position cell =
  let
    ( colours, maybeCommand ) =
      case cell of
        Path ->
          ( [ stroke "grey", fill "#fdfdfd" ]
          , Just (WalkTo position)
          )

        Block ->
          ( [ stroke "#0466da", fill "#04c9da" ]
          , Just (WalkTo position)
          )

        Character ->
          ( [ stroke "#8504da", fill "#e4049a" ]
          , Nothing
          )

        Thing object ->
          ( [ stroke "black", fill "yellow" ]
          , Just (Interact position (Thing object))
          )
  in
    rect
      ([ x (toString (fst position * tileSize))
       , y (toString (snd position * tileSize))
       , width (px tileSize)
       , height (px tileSize)
       , onMouseOut (Signal.message address (Hint Nothing))
       , onMouseOver (Signal.message address (Hint maybeCommand))
       ]
        ++ colours
        ++ (case maybeCommand of
              Nothing ->
                []

              Just command ->
                [ onClick (Signal.message address (PlayerCommand command))
                ]
           )
      )
      []

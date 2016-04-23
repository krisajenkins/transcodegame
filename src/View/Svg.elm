module View.Svg (root, tileSize) where

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
  45


root : Address Action -> Model -> Html
root address model =
  svg
    [ width (px (18 * tileSize))
    , height (px (11 * tileSize))
    ]
    [ g
        []
        (model.world
          |> Dict.toList
          |> List.map (uncurry (tile address))
        )
    , tile address model.player.position (Thing ThePlayer)
    ]


tile : Address Action -> Position -> Cell -> Svg
tile address position cell =
  let
    ( preamble, colours, maybeCommand ) =
      case cell of
        Path ->
          ( defs [] []
          , [ stroke "grey", fill "#fdfdfd" ]
          , Just (InteractAt position Path)
          )

        Block ->
          ( defs [] []
          , [ stroke "#0466da", fill "#04c9da" ]
          , Just (InteractAt position Block)
          )

        Thing ThePlayer ->
          ( (patternDefs (toString ThePlayer))
          , [ stroke "#8504da", fill "url(#ThePlayer)" ]
          , Just (InteractAt position (Thing ThePlayer))
          )

        Thing object ->
          ( (patternDefs (toString object))
          , [ stroke "black", fill ("url(#" ++ toString object ++ ")") ]
          , Just (InteractAt position (Thing object))
          )
  in
    g
      []
      [ preamble
      , rect
          ([ x (toString (fst position * tileSize))
           , y (toString (snd position * tileSize))
           , width (px tileSize)
           , height (px tileSize)
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
      ]


patternDefs : String -> Svg
patternDefs objectName =
  defs
    []
    [ pattern
        [ id objectName
        , patternUnits "userSpaceOnUse"
        , width (px tileSize)
        , height (px tileSize)
        ]
        [ image
            [ xlinkHref ("images/" ++ objectName ++ ".png")
            , x (px 0)
            , y (px 0)
            , width (px tileSize)
            , height (px tileSize)
            ]
            []
        ]
    ]

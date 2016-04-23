module View (root) where

import Signal exposing (..)
import Html exposing (Html)
import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Common.View exposing (..)
import View.Svg


root : Address Action -> Model -> Html
root address model =
  div
    [ style [ ( "margin", "0 100px" ) ]
    ]
    [ div
        [ style
            [ ( "perspective", px 1000 )
            , ( "width", px 800 )
            , ( "height", px 300 )
            ]
        ]
        [ div
            [ style
                [ ( "transform", "rotate3d(1,0,0,45deg)" )
                , ( "width", pct 100 )
                , ( "height", pct 100 )
                ]
            ]
            [ View.Svg.root address model ]
        ]
    , div
        [ style
            [ ( "font-size", px 24 )
            ]
        ]
        [ text (Maybe.withDefault "" model.dialogue) ]
    , hintView model.hint
    , h4
        []
        (case model.player.inventory of
          [] ->
            [ text "I, I who have nothing." ]

          xs ->
            List.map (\o -> div [] [ text o.name ]) xs
        )
    , button
        []
        [ text "Pick up" ]
    ]


hintView : Maybe a -> Html
hintView hint =
  h3
    []
    [ text
        (case hint of
          Nothing ->
            " "

          Just command ->
            (toString command)
        )
    ]

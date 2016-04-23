module View (root) where

import Common.View exposing (..)
import Narrative
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import Types exposing (..)
import View.Svg


root : Address Action -> Model -> Html
root address model =
  div
    [ style
        [ ( "width", (px (17 * View.Svg.tileSize)) )
        , ( "height", (px (8 * View.Svg.tileSize)) )
        , ( "margin", "0 auto" )
        ]
    ]
    [ div
        [ style
            [ ( "perspective", px 1000 )
            , ( "text-align", "center" )
            ]
        ]
        [ div
            [ style
                [ ( "transform", "rotate3d(1,0,0,35deg)" )
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
    , div
        [ class "btn-group" ]
        (List.map
          (commandButton address)
          [ ( PartialCommand PartialPickUp, "Pick up" )
          , ( PartialCommand PartialExamine, "Examine" )
          , ( PartialCommand PartialUse, "Use" )
          ]
        )
    , inventoryView address model.player.inventory
    , hintView model.hint
    , div
        [ class "alert alert-info" ]
        [ code
            []
            [ text
                (case model.partialCommand of
                  Nothing ->
                    ""

                  Just PartialPickUp ->
                    "Pick up..."

                  Just PartialExamine ->
                    "Examine..."

                  Just PartialUse ->
                    "Use..."

                  Just (PartialUseOne thing) ->
                    "Use " ++ Narrative.nameOf thing ++ " with..."
                )
            ]
        ]
    ]


hintView : Maybe String -> Html
hintView hint =
  case hint of
    Nothing ->
      span [] []

    Just string ->
      h3
        [ class "alert alert-info" ]
        [ text string ]


inventoryView : Address Action -> List Object -> Html
inventoryView address inventory =
  div
    []
    [ h4
        []
        [ text "Inventory" ]
    , div [] (List.map (inventoryObjectView address) inventory)
    ]


inventoryObjectView : Address Action -> Object -> Html
inventoryObjectView address object =
  button
    [ class "btn btn.info"
    , onClick address (PlayerCommand (Interact (Thing object)))
    ]
    [ text (toString object) ]


commandButton : Address Action -> ( Command, String ) -> Html
commandButton address ( command, title ) =
  button
    [ class "btn btn-lg btn-info"
    , onClick address (PlayerCommand command)
    ]
    [ text title ]

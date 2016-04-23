module Narrative (..) where

import Types exposing (..)


handleCommand : Command -> Model -> ( Model, Maybe String )
handleCommand command model =
  case command of
    WalkTo newDestination ->
      if canStandOn (objectAt model.world newDestination) then
        ( { model
            | destination = Just newDestination
            , partialCommand = Nothing
          }
        , Just "Chaaaaarrrrrrge!"
        )
      else
        ( model
        , Just "Hmmm...that looks like that would hurt."
        )

    PartialCommand partial ->
      ( { model | partialCommand = Just partial }
      , Just
          (case partial of
            PartialPickUp ->
              "Okay, what shall I pick up?"

            PartialExamine ->
              "Okay, what shall I examine?"

            PartialUse ->
              "Okay, what shall I use?"

            PartialUseOne _ ->
              "Okay, what shall I use it with?"
          )
      )

    PickUp Cinzano ->
      ( model, Just "Dear lord, I'm not drinking that." )

    _ ->
      ( model, Just "I'm sorry, I can't do that." )


handleHint : Command -> Model -> Maybe String
handleHint command model =
  case command of
    WalkTo position ->
      Just
        ("Walk to " ++ toString position)

    _ ->
      Nothing

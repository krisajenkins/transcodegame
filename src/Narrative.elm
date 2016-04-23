module Narrative (..) where

import Types exposing (..)


handleCommand : Command -> Model -> ( Model, Maybe String )
handleCommand command model =
  case command of
    WalkTo newDestination ->
      if canStandOn (objectAt model.world newDestination) then
        ( { model | destination = Just newDestination }
        , Just "Chaaaaarrrrrrge!"
        )
      else
        ( model
        , Just "Hmmm...that looks like that would hurt."
        )

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

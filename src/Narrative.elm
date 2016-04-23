module Narrative (..) where

import Types exposing (..)
import Dict


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

    PickUp position Cinzano ->
      let
        player =
          model.player

        newModel =
          { model
            | world = Dict.insert position Path model.world
            , player = { player | inventory = Cinzano :: player.inventory }
          }
      in
        ( newModel, Just "Just don't make me drink it." )

    Examine Cinzano ->
      ( model, Just "The party isn't over 'til there only Cinzano left to drink." )

    Interact position (Thing object) ->
      case model.partialCommand of
        Just PartialPickUp ->
          handleCommand (PickUp position object) model

        Just PartialUse ->
          ( { model | partialCommand = Just (PartialUseOne object) }
          , Just "What shall I use it with?"
          )

        Just (PartialUseOne otherObject) ->
          if object == otherObject then
            ( model, Just "I can't use something with itself." )
          else
            handleCommand (Use object otherObject) model

        Just PartialExamine ->
          handleCommand (Examine object) model

        Nothing ->
          handleCommand (Examine object) model

    Examine obj ->
      ( model, Just (examine obj) )

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

examine : Object -> String
examine obj =
  case obj of
    Chicken -> "It's a rubber chicken with a pulley in the middle"
    Cinzano -> "Smells vaguely of petrol.  Why would it ever be half-empty?"
    Molotov -> "Slightly less lethal than its components"
    PotatoSackFull -> "Well, you could boil them?  Or mash them?"
    PotatoSackEmpty -> "I bet those potatoes made a good stew."
    -- TODO To be removed when pattern matching is complete
    --      (or replace with Unknown)
    _ -> "I have no idea what this.  I don't think it knows what it is."

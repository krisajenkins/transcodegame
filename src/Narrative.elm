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

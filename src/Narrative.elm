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

    PickUp position ThePlayer ->
      ( model, Just "That was a real pick-me-up!" )

    PickUp position thing ->
      if canPickUp thing then
        let
          player =
            model.player

          newModel =
            { model
              | world = Dict.insert position Path model.world
              , player = { player | inventory = thing :: player.inventory }
            }
        in
          ( newModel, Just "I've got it." )
      else
        ( model, Just "I can't pick that up." )

    Interact Path ->
      ( model, Nothing )

    Interact Block ->
      ( model, Nothing )

    Interact (Thing object) ->
      case model.partialCommand of
        Just PartialPickUp ->
          ( { model | partialCommand = Nothing }
          , Just "I've already got it."
          )

        Just PartialUse ->
          ( { model | partialCommand = Just (PartialUseOne object) }
          , Just "What shall I use it with?"
          )

        Just (PartialUseOne otherObject) ->
          { model | partialCommand = Nothing }
            |> handleCommand (Use object otherObject)

        Just PartialExamine ->
          { model | partialCommand = Nothing }
            |> handleCommand (Examine object)

        Nothing ->
          handleCommand (Examine object) model

    Use object otherObject ->
      Maybe.oneOf
        [ handleUse object otherObject model
        , handleUse otherObject object model
        ]
        |> Maybe.withDefault ( model, Just "It's not going to work." )

    InteractAt position (Thing object) ->
      case model.partialCommand of
        Just PartialPickUp ->
          { model | partialCommand = Nothing }
            |> handleCommand (PickUp position object)

        _ ->
          handleCommand (Interact (Thing object)) model

    InteractAt position Block ->
      ( model, Nothing )

    InteractAt position Path ->
      ( model, Nothing )

    Examine Cinzano ->
      ( model, Just "The party isn't over 'til there only Cinzano left to drink." )

    Examine obj ->
      ( model, Just (examine obj) )


handleHint : Command -> Model -> Maybe String
handleHint command model =
  case command of
    WalkTo position ->
      Just
        ("Walk to " ++ toString position)

    _ ->
      Nothing


nameOf : Object -> String
nameOf obj =
  case obj of
    ThePlayer ->
      "Myself"

    _ ->
      toString obj


examine : Object -> String
examine obj =
  case obj of
    BlackBiro ->
      "It's a biro. It writes in black ink. This looks perfect for filling in official documents."

    Chicken ->
      "It's a rubber chicken with a pulley in the middle."

    Cinzano ->
      "Smells vaguely of petrol.  Why would it ever be half-empty?"

    Molotov ->
      "Slightly less lethal than its components"

    Package ->
      "The paperwork fits snugly inside the potato sack, making a beautifully wrapped package. None of the paperwork will get lost now."

    Paperwork ->
      "This stack of unfinished paperwork looks more and more intimidating the longer you spend looking at it. You really need to fill it in today."

    PaperworkDone ->
      "The finished paperwork represents hours of work. But you can't send it like this, or most of it will get lost in the post, or eaten by beavers."

    Parcel ->
      "A beautifully wrapped, but slightly enormous, stamped and addressed parcel, ready for posting."

    Postbox ->
      "A shining symbol of the postal system. It has an unusually large opening for parcels. I think you could probably get an elephant through that."

    PotatoSackEmpty ->
      "An empty potato sack. Perfect for filling with heavy things that need to stay together."

    PotatoSackFull ->
      "Boil 'em, mash 'em, stick 'em in a stew."

    Rag ->
      "Upon closer examination, you realise this rag used to be a pair of underwear. Whose underwear? Who knows."

    Shed ->
      "A flimsy wooden shed, the door to which has been stuck for years. There might be something inside, but you can't get to it. The wood looks flammable."

    Stamps ->
      "This looks like just about enough to send a really big parcel. You may die of dehydration whilst sticking them down though."

    UselessVaseEmpty ->
      "This vase is useless. Absolutely useless. Why are you still looking at it?"

    UselessVaseFull ->
      "This is a useless and ugly vase. No wonder you don't have it out on display. For some reason, it seems to be filled with stamps rather than flowers."

    WheelbarrowBroken ->
      "This wheelbarrow doesn't have a working wheel. Might as well just call it a barrow."

    WheelbarrowFixed ->
      "This looks like you could use it to transport something heavy."

    -- TODO To be removed when pattern matching is complete
    --      (or replace with Unknown)
    _ ->
      "I have no idea what this.  I don't think it knows what it is."


handleUse : Object -> Object -> Model -> Maybe ( Model, Maybe String )
handleUse object otherObject model =
  case ( object, otherObject ) of
    ( ThePlayer, ThePlayer ) ->
      Just ( model, Just "You want me to use myself? Naughty." )

    ( Cinzano, ThePlayer ) ->
      Just ( model, Just "There. Is. No. Way. I. Will. Drink. Cinzano." )

    _ ->
      Nothing

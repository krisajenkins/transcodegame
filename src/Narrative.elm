module Narrative (..) where

import Set exposing (Set)
import Types exposing (..)
import Dict


handleCommand : Command -> Model -> ( Model, Maybe String )
handleCommand command model =
  case command of
    WalkTo newDestination ->
      if newDestination /= model.player.position then
        if canStandOn (objectAt model.world newDestination) then
          ( { model
              | destination = Just newDestination
              , partialCommand = Nothing
            }
          , Just "Chaaaaarrrrrrge!"
          )
        else
          let
            neighbours =
              validMovesFrom model.world newDestination
          in
            case List.head (Set.toList neighbours) of
              Nothing ->
                ( model
                , Just "Hmmm...that looks like that would hurt."
                )

              Just pos ->
                handleCommand (WalkTo pos) model
      else
        ( model, Just "I'm already there." )

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
          ( newModel, Just ("I've got: " ++ nameOf thing) )
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
          { model
            | partialCommand = Nothing
            , queuedCommand = Just (PickUp position object)
          }
            |> handleCommand (WalkTo position)

        _ ->
          handleCommand (Interact (Thing object)) model

    InteractAt position Block ->
      (case model.partialCommand of
        Just PartialPickUp ->
          ( { model | partialCommand = Nothing }
          , Just "Yeah, tear down the walls! AnARcHy!!1!"
          )

        Just PartialExamine ->
          ( { model | partialCommand = Nothing }
          , Just "It's a wall."
          )

        _ ->
          handleCommand (WalkTo position) model
      )

    InteractAt position Path ->
      (case model.partialCommand of
        Just PartialPickUp ->
          ( { model | partialCommand = Nothing }
          , Just "DIY was never my strong suit."
          )

        Just PartialExamine ->
          ( { model | partialCommand = Nothing }
          , Just "It's the floor."
          )

        _ ->
          handleCommand (WalkTo position) model
      )

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
    BlackBiro ->
      "Black Biro"

    Chicken ->
      "Chicken"

    Cinzano ->
      "Cinzano"

    Fridge ->
      "Fridge"

    FridgeEmpty ->
      "Empty Fridge"

    Lighter ->
      "Lighter"

    Molotov ->
      "Molotov Cocktail"

    MolotovLit ->
      "Lit Molotov Cocktail"

    Package ->
      "Package"

    Paperwork ->
      "Paperwork"

    PaperworkDone ->
      "Completed Paperwork"

    Parcel ->
      "Parcel"

    Postbox ->
      "Postbox"

    PotatoSackEmpty ->
      "Empty Potato Sack"

    PotatoSackFull ->
      "Full Potato Sack"

    Rag ->
      "Rag"

    Shed ->
      "Shed"

    Stamps ->
      "Stamps"

    Still ->
      "Still"

    UselessVaseEmpty ->
      "Useless Empty Vase"

    UselessVaseFull ->
      "Useless Vase"

    WheelbarrowBroken ->
      "Broken Wheelbarrow"

    WheelbarrowFixed ->
      "Wheelbarrow"

    WheelbarrowFull ->
      "Loaded Wheelbarrow"

    ThePlayer ->
      "Myself"


examine : Object -> String
examine obj =
  case obj of
    BlackBiro ->
      "It's a biro. It writes in black ink. This looks perfect for filling in official documents."

    Chicken ->
      "It's a rubber chicken with a pulley in the middle."

    Cinzano ->
      "Smells vaguely of petrol.  Why would it ever be half-empty?"

    Fridge ->
      "You open the fridge and bask in the sickly glow of the interior. Looking closely, you notice the ugliest vase you have ever seen, and for some reason, a biro embedded in the butter dish."

    FridgeEmpty ->
      "It looks just the same, minus random stationary and hideous glassware."

    Lighter ->
      "Probably good for lighting things on fire. Just a guess."

    Molotov ->
      "Slightly less lethal than its components"

    MolotovLit ->
      "Might wanna throw this in the next few minutes. Just an idea. Maybe at something that will burn."

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

    Still ->
      "This looks perfect for making potato moonshine."

    UselessVaseEmpty ->
      "This vase is useless. Absolutely useless. Why are you still looking at it?"

    UselessVaseFull ->
      "This is a useless and ugly vase. No wonder you don't have it out on display. For some reason, it seems to be filled with stamps rather than flowers."

    WheelbarrowBroken ->
      "This wheelbarrow doesn't have a working wheel. Might as well just call it a barrow."

    WheelbarrowFixed ->
      "This looks like you could use it to transport something heavy."

    WheelbarrowFull ->
      "Your precious paperwork, all packaged and ready to roll. Literally."

    ThePlayer ->
      "Hey, I look great today! Especially for somebody who had way too many Cinzanos last night."


handleUse : Object -> Object -> Model -> Maybe ( Model, Maybe String )
handleUse object otherObject model =
  case ( object, otherObject ) of
    ( ThePlayer, ThePlayer ) ->
      Just ( model, Just "You want me to use myself? Naughty." )

    ( UselessVaseFull, _ ) ->
      Just ( model, Just "What part of the word 'useless' do you not understand?" )

    ( UselessVaseEmpty, _ ) ->
      Just ( model, Just "What part of the word 'useless' do you not understand?" )

    ( BlackBiro, Paperwork ) ->
      Just
        ( combineItems BlackBiro Paperwork PaperworkDone model
        , Just "The black biro allows you to fill out the stack of paperwork after a while.  A very, very long while."
        )

    ( Rag, Cinzano ) ->
      Just
        ( combineItems Rag Cinzano Molotov model
        , Just "You turn the half empty bottle into what looks like a crude molotov cocktail.  This is probably a better use for a half empty bottle of Cinzano."
        )

    ( Lighter, Shed ) ->
      Just ( model, Just "The wood may look flammable, but it'll take more than the lighter to set it on fire." )

    ( Lighter, Paperwork ) ->
      Just ( model, Just "This is not the correct way to get out your anger at bureaucracy." )

    ( Lighter, PaperworkDone ) ->
      Just ( model, Just "This is not the correct way to get out your anger at bureaucracy." )

    ( Lighter, Molotov ) ->
      Just
        ( combineItems Lighter Molotov MolotovLit model
        , Just "Now it looks even more dangerous."
        )

    ( MolotovLit, Shed ) ->
      Just
        ( removeItems [ MolotovLit ] model
        , Just "You throw the molotov at the shed and watch as it burns to the ground.  You shed hating monster.  The one thing still standing in the wreckage is a broken wheelbarrow."
        )

    ( Still, PotatoSackFull ) ->
      Just
        ( removeItems [ PotatoSackFull ] model
        , Just "You empty the potatoes into the still and find yourself with an empty potato sack."
        )

    ( PotatoSackEmpty, PaperworkDone ) ->
      Just
        ( combineItems PotatoSackEmpty PaperworkDone Package model
        , Just "Sticking the paperwork into the potato sack makes what could just about pass as a package.  Good job!"
        )

    ( Stamps, Package ) ->
      Just
        ( combineItems Stamps Package Parcel model
        , Just "It takes a lot of licking, but you cover the parcel in stamps eventually.  Might need a drinks break before you do anything else though."
        )

    ( Parcel, WheelbarrowFixed ) ->
      Just
        ( combineItems Parcel WheelbarrowFixed WheelbarrowFull model
        , Just "You pick up the parcel and place it into the wheelbarrow.  Then you place the wheelbarrow into your pocket, not for one minute questioning the laws of logic and physics in this universe."
        )

    ( WheelbarrowFull, Postbox ) ->
      Just
        ( addItem WheelbarrowFixed (removeItems [ WheelbarrowFull ] model)
        , Just "Emptying the contents of the wheelbarrow into the postbox, you give a sigh of relief.  Now all that there's left to do is to wait for it to be processed, and pray that bureaucracy will be merciful on you."
        )

    ( Lighter, PotatoSackFull ) ->
      Just ( model, Just "You might be able to make roast potatoes this way, but it'll take a really long time." )

    ( Cinzano, ThePlayer ) ->
      Just ( model, Just "There. Is. No. Way. I. Will. Drink. Cinzano." )

    _ ->
      Nothing


combineItems : Object -> Object -> Object -> Model -> Model
combineItems obj1 obj2 result model =
  addItem result (removeItems [ obj1, obj2 ] model)


addItem : Object -> Model -> Model
addItem item model =
  let
    newInventory =
      item :: model.player.inventory

    player =
      model.player

    newPlayer =
      { player | inventory = newInventory }
  in
    { model | player = newPlayer }


removeItems : List Object -> Model -> Model
removeItems items model =
  let
    newInventory =
      List.filter (flip List.member items) model.player.inventory

    player =
      model.player

    newPlayer =
      { player | inventory = newInventory }
  in
    { model | player = newPlayer }

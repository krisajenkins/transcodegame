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
        ( model, Just (pickUpFail thing) )

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

    Examine UselessVaseFull ->
      ( model
          |> removeItems [ UselessVaseFull ]
          |> addItems [ UselessVaseEmpty, Stamps ]
      , Just (examine UselessVaseFull)
      )

    Examine Fridge ->
      let
        newInventory =
          model
            |> addItems [ BlackBiro, UselessVaseFull ]
      in
        ( { newInventory | world = Dict.insert ( 11, 2 ) (Thing FridgeEmpty) newInventory.world }
        , Just (examine Fridge)
        )

    Examine obj ->
      ( model, Just (examine obj) )


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
      "You open the fridge and bask in the sickly glow of the interior. Looking closely, you notice the ugliest vase you have ever seen, and for some reason, biros embedded in the butter dish."

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
        ( { model | world = Dict.insert ( 7, 7 ) (Thing PaperworkDone) model.world }
            |> removeItems [ BlackBiro, Paperwork ]
        , Just "The black biro allows you to fill out the stack of paperwork after a while.  A very, very long while."
        )

    ( Rag, Cinzano ) ->
      Just
        ( combineItems Rag Cinzano Molotov model
        , Just "You turn the half empty bottle into what looks like a crude molotov cocktail.  This is probably a better use for a half empty bottle of Cinzano."
        )

    ( Lighter, second ) ->
      case second of
        Shed ->
          Just ( model, Just "The wood may look flammable, but it'll take more than the lighter to set it on fire." )

        Paperwork ->
          Just ( model, Just "This is not the correct way to get out your anger at bureaucracy." )

        PaperworkDone ->
          Just ( model, Just "This is not the correct way to get out your anger at bureaucracy." )

        Molotov ->
          Just ( combineItems Lighter Molotov MolotovLit model, Just "Now it looks even more dangerous." )

        PotatoSackFull ->
          Just ( model, Just "You might be able to make roast potatoes this way, but it'll take a really long time." )

        _ ->
          Nothing

    ( MolotovLit, Shed ) ->
      let
        newInv =
          removeItems [ MolotovLit ] model

        newModel =
          { newInv | world = Dict.insert ( 16, 5 ) (Thing WheelbarrowBroken) model.world }
      in
        Just
          ( newModel
          , Just "You throw the molotov at the shed and watch as it burns to the ground.  You shed hating monster.  The one thing still standing in the wreckage is a broken wheelbarrow."
          )

    ( WheelbarrowBroken, Chicken ) ->
      Just
        (model
          |> removeItems [ Chicken ]
          |> handleCommand (PickUp ( 16, 5 ) WheelbarrowFixed)
        )

    ( Still, PotatoSackFull ) ->
      Just
        ( model
            |> removeItems [ PotatoSackFull ]
            |> addItems [ PotatoSackEmpty ]
        , Just "You empty the potatoes into the still and find yourself with an empty potato sack."
        )

    ( PotatoSackEmpty, PaperworkDone ) ->
      Just
        ( { model | world = Dict.insert ( 7, 7 ) (Thing Package) model.world }
            |> removeItems [ PotatoSackEmpty ]
        , Just "Sticking the paperwork into the potato sack makes what could just about pass as a package.  Good job!"
        )

    ( Stamps, Package ) ->
      Just
        ( { model | world = Dict.insert ( 7, 7 ) (Thing Parcel) model.world }
            |> removeItems [ Stamps ]
        , Just "It takes a lot of licking, but you cover the parcel in stamps eventually.  Might need a drinks break before you do anything else though."
        )

    ( Parcel, WheelbarrowFixed ) ->
      let
        ( newModel, _ ) =
          handleCommand (PickUp ( 7, 7 ) WheelbarrowFull) model
      in
        Just ( { model | world = Dict.insert ( 7, 7 ) Path model.world }
                 |> addItems [ WheelbarrowFull ]
                 |> removeItems [ WheelbarrowFixed ]
             , Just "You pick up the parcel and place it into the wheelbarrow.  Then you place the wheelbarrow into your pocket, not for one minute questioning the laws of logic and physics in this universe."
             )

    ( WheelbarrowFull, Postbox ) ->
      Just
        ( model
            |> removeItems [ WheelbarrowFull ]
            |> addItems [ WheelbarrowFixed ]
        , Just "Emptying the contents of the wheelbarrow into the postbox, you give a sigh of relief.  Now all that there's left to do is to wait for it to be processed, and pray that bureaucracy will be merciful on you."
        )

    ( Cinzano, ThePlayer ) ->
      Just ( model, Just "There. Is. No. Way. I. Will. Drink. Cinzano." )

    _ ->
      Nothing

pickUpFail : Object -> String
pickUpFail object =
  case object of
    ThePlayer -> "That was a real pick-me-up!"
    -- TODO review or rewrite temporary "can't pick this up" messages
    Fridge -> "I'd love to bring a fridge full of food with me, but that's not really possible."
    FridgeEmpty -> "There's not even any food in this.  I can't be bothered to take it with me."
    Paperwork -> "That must weigh a tonne!  Besides, I need to fill the forms out first..."
    PaperworkDone -> "I should package this ready to send before I try to move this."
    Package -> "I should put stamps on this first."
    Postbox -> "I could try, but then how would I send the forms?"
    Shed -> "It's a shed.  That won't fit in my pocket."
    Still -> "The still remains very still no matter how much I try to move it."
    WheelbarrowBroken -> "It's missing a wheel; I can't move that."

    _ -> "I can't pick that up."

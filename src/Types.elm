module Types (..) where

import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Time)


type alias Position =
  ( Int, Int )


type alias World a =
  Dict Position a


type Object
  = BlackBiro
  | Chicken
  | Cinzano
  | Fridge
  | FridgeEmpty
  | Lighter
  | Molotov
  | MolotovLit
  | Package
  | Paperwork
  | PaperworkDone
  | Parcel
  | Postbox
  | PotatoSackEmpty
  | PotatoSackFull
  | Rag
  | Shed
  | Stamps
  | Still
  | UselessVaseEmpty
  | UselessVaseFull
  | WheelbarrowBroken
  | WheelbarrowFixed
  | WheelbarrowFull
  | ThePlayer


type Cell
  = Block
  | Path
  | Thing Object


type PartialCommand
  = PartialPickUp
  | PartialExamine
  | PartialUse
  | PartialUseOne Object


type Command
  = WalkTo Position
  | PickUp Position Object
  | Examine Object
  | Use Object Object
  | Interact Cell
  | InteractAt Position Cell
  | PartialCommand PartialCommand


type Action
  = Tick Time
  | PlayerCommand Command
  | Hint (Maybe Command)


type alias Player =
  { position : Position, inventory : List Object }


type alias Model =
  { world : World Cell
  , player : Player
  , dialogue : Maybe String
  , hint : Maybe String
  , partialCommand : Maybe PartialCommand
  , destination : Maybe Position
  , queuedCommand : Maybe Command
  , timeSinceLastMove : Maybe Time
  }


canStandOn : Maybe Cell -> Bool
canStandOn cell =
  case cell of
    Nothing ->
      False

    Just Path ->
      True

    Just (Thing _) ->
      False

    Just Block ->
      False


objectAt : World Cell -> Position -> Maybe Cell
objectAt =
  flip Dict.get


estimatedDistance : Position -> Position -> Int
estimatedDistance ( x1, y1 ) ( x2, y2 ) =
  (abs (x1 - x2))
    + (abs (y1 - y2))


movesFrom : Position -> Set Position
movesFrom ( x, y ) =
  Set.fromList
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x - 1, y + 1 )
    , ( x - 1, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y - 1 )
    ]


validMovesFrom : World Cell -> Position -> Set Position
validMovesFrom world position =
  Set.filter
    (canStandOn << objectAt world)
    (movesFrom position)


canPickUp : Object -> Bool
canPickUp object =
  case object of
    BlackBiro ->
      True

    Chicken ->
      True

    Cinzano ->
      True

    Molotov ->
      True

    Package ->
      False

    Paperwork ->
      False

    PaperworkDone ->
      False

    Parcel ->
      False

    Postbox ->
      False

    PotatoSackEmpty ->
      True

    PotatoSackFull ->
      True

    Rag ->
      True

    Shed ->
      False

    Stamps ->
      True

    UselessVaseEmpty ->
      True

    UselessVaseFull ->
      True

    WheelbarrowBroken ->
      False

    WheelbarrowFixed ->
      True

    WheelbarrowFull ->
      True

    ThePlayer ->
      False

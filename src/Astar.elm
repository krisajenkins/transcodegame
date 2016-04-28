module Astar (findPath, AstarResult(..), Position, Path) where

import Set exposing (Set)
import Dict exposing (Dict)
import Array exposing (Array)


type alias Position =
  ( Int, Int )


type alias Path =
  Array Position


type alias Model =
  { start : Position
  , evaluated : Set Position
  , openSet : Set Position
  , costs : Dict Position Float
  , cameFrom : Dict Position Position
  }

-- The result of the modified A* algorithm: either a shortest path to
-- the goal position, or the nearest reachable position to the goal
type AstarResult = Success Path | Failure Position

initialModel : Position -> Model
initialModel start =
  { start = start
  , evaluated = Set.empty
  , openSet = Set.singleton start
  , costs = Dict.singleton start 0
  , cameFrom = Dict.empty
  }


cheapestOpen : (Position -> Float) -> Model -> Maybe Position
cheapestOpen costFn model =
  model.openSet
    |> Set.toList
    |> List.filterMap
        (\position ->
          case Dict.get position model.costs of
            Nothing ->
              Nothing

            Just cost ->
              Just ( position, cost + costFn position )
        )
    |> List.sortBy snd
    |> List.head
    |> Maybe.map fst


reconstructPath : Dict Position Position -> Position -> Path
reconstructPath cameFrom goal =
  case Dict.get goal cameFrom of
    Nothing ->
      Array.empty

    Just next ->
      Array.push
        goal
        (reconstructPath cameFrom next)


updateCost : Position -> Position -> Model -> Model
updateCost current neighbour model =
  let
    newCameFrom =
      Dict.insert neighbour current model.cameFrom

    distanceTo =
      reconstructPath newCameFrom neighbour
        |> Array.length
        |> toFloat

    newModel =
      { model
        | costs = Dict.insert neighbour distanceTo model.costs
        , cameFrom = newCameFrom
      }
  in
    case Dict.get neighbour model.costs of
      Nothing ->
        newModel

      Just previousDistance ->
        if distanceTo < previousDistance then
          newModel
        else
          model

guessClosestNeighbour : (Position -> Float) -> Model -> Position
guessClosestNeighbour costFn model =
  let
    tagWithCosts p =
      case Dict.get p model.costs of
        Nothing -> Nothing
        Just c -> Just ( p, costFn p, c )

    projectCosts pos =
      case pos of
        ( _, estimated, actual ) -> ( estimated, actual )

    fst3 tup =
      case tup of
        ( a, _, _ ) -> a
  in
    model.evaluated
      |> Set.toList
      |> List.filterMap tagWithCosts
      |> List.sortBy projectCosts
      |> List.head
      |> Maybe.map fst3
      |> Maybe.withDefault model.start

astar : (Position -> Position -> Float) -> (Position -> Set Position) -> Position -> Model -> AstarResult
astar costFn moveFn goal model =
  case cheapestOpen (costFn goal) model of
    Nothing ->
      Failure (guessClosestNeighbour (costFn goal) model)

    Just current ->
      if current == goal then
        Success (reconstructPath model.cameFrom goal)
      else
        let
          modelPopped =
            { model
              | openSet = Set.remove current model.openSet
              , evaluated = Set.insert current model.evaluated
            }

          neighbours =
            moveFn current

          newNeighbours =
            Set.diff neighbours modelPopped.evaluated

          modelWithNeighbours =
            { modelPopped
              | openSet =
                  Set.union
                    modelPopped.openSet
                    newNeighbours
            }

          modelWithCosts =
            Set.foldl (updateCost current) modelWithNeighbours newNeighbours
        in
          astar costFn moveFn goal modelWithCosts

{-| Find a path between `start` and `end`. You must supply a cost function and a move function.

  The cost function must estimate the distance between any two
  positions. It doesn't really matter how accurate this estimate is,
  as long as it _never_ underestimates.

  The move function takes a `Position` and returns a `Set` of possible
  places you can move to in one step.

  If this function returns `Nothing`, there is no path between the two
  points. Otherwise it returns `Just` an `Array` of steps from `start`
  to `end`.
-}
findPath : (Position -> Position -> Float) -> (Position -> Set Position) -> Position -> Position -> AstarResult
findPath costFn moveFn start end =
  initialModel start
    |> astar costFn moveFn end

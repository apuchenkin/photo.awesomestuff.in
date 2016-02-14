module Lib.Helpers (singleton, noFx, chainAction, combineParams, getPath) where

import Effects
import Dict
import Lib.Types exposing (ActionEffects, Response (..), Action, RouteParams, Route)
import MultiwayTree   exposing (Tree, Forest)
import MultiwayTreeUtil exposing (lca, treeLookup, traverseFrom)

singleton : a -> List a
singleton action = [ action ]

noFx : state -> (state, ActionEffects state)
noFx state = (state, Effects.none)

chainAction : Action state -> Action state -> Action state
chainAction action1 action2 state =
  let
    (Response (state', effects)) = action1 state
    (Response (state'', effects')) = action2 state'
  in Response (state'', Effects.batch [effects, effects'])

combineParams : RouteParams -> Route route -> Route route
combineParams dict (route, params) = (route, Dict.union params dict)

-- path from node a to node b in the forest
getPath : Maybe a -> a -> Forest a -> List a
getPath from to forest = Maybe.withDefault []
  <| flip Maybe.map (List.head <| List.filterMap (\tree -> treeLookup to tree) forest)
  <| \zipper ->
    let from' = from `Maybe.andThen` (\from' -> lca from' to forest)
    in traverseFrom from' zipper

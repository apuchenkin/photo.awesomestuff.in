module Lib.Helpers where

import Effects
import Dict
import Lib.Types exposing (ActionEffects, Response (..), Action, RouteParams, Route)
import MultiwayTree   exposing (Tree, Forest)
import MultiwayTreeUtil exposing (lca, treeLookup, traverse)

singleton : a -> List a
singleton action = [ action ]

noFx : state -> (state, ActionEffects state)
noFx state = (state, Effects.none)

combineParams : RouteParams -> Route route -> Route route
combineParams dict (route, params) = (route, Dict.union params dict)

-- path from node a to node b in the forest
getPath : a -> Forest a -> List a
getPath route forest = Maybe.withDefault []
  <| flip Maybe.map (List.head <| List.filterMap (\tree -> treeLookup route tree) forest)
  <| \zipper -> traverse zipper

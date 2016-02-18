module Lib.Helpers where

import Effects
import Dict
import Lib.Types exposing (ActionEffects, Response (..), Action, RouteParams, Route)

singleton : a -> List a
singleton action = [ action ]

noFx : state -> (state, ActionEffects state)
noFx state = (state, Effects.none)

combineParams : RouteParams -> Route route -> Route route
combineParams dict (route, params) = (route, Dict.union params dict)

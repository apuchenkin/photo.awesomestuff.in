module Lib.Helpers (singleton, noFx, chainAction) where

import Effects
import Lib.Types exposing (ActionEffects, Response (..), Action)

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

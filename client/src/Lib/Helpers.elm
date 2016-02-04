module Lib.Helpers (singleton, noFx) where

import Effects
import Lib.Types exposing (ActionEffects)

singleton : a -> List a
singleton action = [ action ]

noFx : state -> (state, ActionEffects state)
noFx state = (state, Effects.none)

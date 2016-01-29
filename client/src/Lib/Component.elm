module Lib.Component ( Component, Result, dispatch ) where

import Html exposing (Html)
import Task
import Effects exposing (Effects, Never)

type alias Component state action =
    { init    : (state, Effects action)
    , update  : action -> state -> (state, Effects action)
    , view    : Signal.Address action -> state -> Html
    , inputs  : List (Signal.Signal action)
    }

type alias Result state =
    { html  : Signal Html
    , state : Signal state
    , tasks : Signal (Task.Task Never ())
    }

dispatch : Component state action -> Result state
dispatch component =
    let
        -- singleton : action -> List action
        singleton action = [ action ]

        -- messages : Signal.Mailbox (List action)
        messages = Signal.mailbox []

        -- address : Signal.Address action
        address = Signal.forwardTo messages.address singleton

        -- updateStep : action -> (state, Effects action) -> (state, Effects action)
        updateStep action (state, effects) =
            let
                (state', effects') = component.update action state
            in
                (state', Effects.batch [effects, effects'])

        -- update : List action -> (state, Effects action) -> (state, Effects action)
        update actions (state, _) = List.foldl updateStep (state, Effects.none) actions

        -- inputs : Signal (List action)
        inputs = Signal.mergeMany (messages.signal :: List.map (Signal.map singleton) component.inputs)

        -- result : Signal (state, Effects action)
        result = Signal.foldp update component.init inputs

        state = Signal.map fst result
    in
        { html = Signal.map (component.view address) state
        , state = state
        , tasks = Signal.map (Effects.toTask messages.address << snd) result
        }

module Lib.Router where


import Task
import String
import Array
import Html     exposing (Html, text, div)
import Effects  exposing (Effects, Never)
import History  exposing (path)
import RouteParser exposing (..)
import MultiwayTree exposing (..)

type alias Result state =
    { html  : Signal Html
    , state : Signal state
    , tasks : Signal (Task.Task Never ())
    }

type alias RouterState = {
    state:  Maybe String,
    handlers: List (Handler (List String))
  }

-- transitionTo : Maybe Route -> Route
-- transitionTo mfrom to =

type alias HandlerMap state route = route -> Handler state

pathHandlers : Tree (Matcher route) -> HandlerMap state route -> String -> List (Handler state)
pathHandlers routes rmap url =
  let
    mapSegment s (forest, acc) =
      let
        maybeRoute = match (List.indexedMap (\i v -> mapMatcher (\m -> (i,m)) (datum v)) forest) s
        forest' = Maybe.withDefault [] <| Maybe.andThen maybeRoute (\m -> Maybe.map children (Array.get (fst m) (Array.fromList forest)))
        acc' = acc ++ [Maybe.map snd maybeRoute]
      in (forest', acc')

    maybeRoutes = snd
        <| List.foldl mapSegment ([routes], [])
        -- <| List.filter (not << String.isEmpty)
        <| String.split "/" url

    handlers = List.filterMap (\r -> Maybe.map rmap r) maybeRoutes

  in handlers


runRouter : RouterConfig state route -> Result state
runRouter config =
  let
    -- singleton : action -> List action
    singleton action = [ action ]

    mailbox : Signal.Mailbox (List (Action state))
    mailbox = Signal.mailbox []

    -- address : Signal.Address action
    address = Signal.forwardTo mailbox.address singleton

    updateStep action (state, effects) =
        let
            (state', effects') = toState <| action state
        in
            (state', Effects.batch [effects, effects'])

    update actions = List.foldl updateStep (config.init, Effects.none) actions

    signalHandlers = Signal.map (pathHandlers config.routes config.handlerMap) path

    inputs = Signal.mergeMany <|
         Signal.map (\h -> List.foldl ((++) << .inputs) [] h) signalHandlers
      :: mailbox.signal
      :: List.map (Signal.map singleton) config.inputs

    views = Signal.map (List.map .view) signalHandlers

    -- update' actions (s,_) = List.foldl updateStep ([], Effects.none) actions
    -- result = Signal.foldp update' (["f"], Effects.none) inputs
    result = Signal.map update inputs

    state = Signal.map fst result

    -- renderView : (Signal.Address (Action state) -> state -> Maybe Html -> Maybe Html) -> Maybe Html -> Maybe Html
    -- renderView view parsed = view address state parsed

    html  = Signal.map2 (\state viewList -> Maybe.withDefault (text "error") <| List.foldr (\view parsed -> view address state parsed) Nothing viewList) state views
  in
    {
      html  = html
    , state = state
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }

type alias RouterConfig state route = {
  init:       state,
  handlerMap: HandlerMap state route,
  routes:     Tree (Matcher route),
  inputs:     List (Signal.Signal (Action state))
}

type DirtyState state = DirtyState (state, Effects (Action state))

toState : DirtyState state -> (state, Effects (Action state))
toState (DirtyState s) = s

type alias Action state = state -> DirtyState state

type alias Handler state = {
    view    : Signal.Address (Action state) -> state -> Maybe Html -> Maybe Html
  , inputs  : List (Action state)
  }

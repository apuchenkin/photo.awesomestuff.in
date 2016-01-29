module Lib.Router where

import Json.Decode
import Http
import Task
import String
import Array
import Html     exposing (Html, text, div)
import Effects  exposing (Effects, Never)
import History  exposing (path, setPath)
import RouteParser exposing (..)
import MultiwayTree exposing (..)

type Route = Home | Admin AdminRoute

matchers =
  [ static Home "/" ] ++ (mapMatchers Admin adminMatchers)

-- can be delegated to a component without knowdedge of global routing:

type AdminRoute = Dashboard | Users

adminMatchers =
  [ static Dashboard "/admin", static Users "/users" ]

routes = Tree (static Home "home") [Tree (static (Admin Dashboard) "admin") [], Tree (static (Admin Users) "users") []]

type alias SignalResult state =
    { html  : Signal Html
    , state : Signal state
    , tasks : Signal (Task.Task Never ())
    }

type alias Result state =
    { html  : Html
    , state : state
    , effects : Effects.Effects (Task.Task Never ())
    }

runRouter : Signal String -> SignalResult (List String)
runRouter pathSignal =
  let
    pathHandlers : String -> List (Handler (List String))
    pathHandlers path =
      let
        mapSegment s (forest, acc) =
          let
            maybeRoute = match (List.indexedMap (\i v -> mapMatcher (\m -> (i,m)) (datum v)) forest) s
            forest' = Maybe.withDefault [] <| Maybe.andThen maybeRoute (\m -> Maybe.map children (Array.get (fst m) (Array.fromList forest)))
            acc' = acc ++ [Maybe.map snd maybeRoute]
          in (forest', acc')

        maybeRoutes = snd
            <| List.foldl mapSegment ([routes], [])
            <| List.filter (not << String.isEmpty)
            <| String.split "/" path

        handlers = List.map mapHandler maybeRoutes

      in handlers

    -- singleton : action -> List action
    singleton action = [ action ]

    messages : Signal.Mailbox (List (Action (List String)))
    messages = Signal.mailbox []

    -- address : Signal.Address action
    address = Signal.forwardTo messages.address singleton

    updateStep action (state, effects) =
        let
            (state', effects') = toState <| action state
        in
            (state', Effects.batch [effects, effects'])

    update actions = List.foldl updateStep ([], Effects.none) actions

    signalHandlers = Signal.map pathHandlers pathSignal

    inputs = Signal.merge (Signal.map (\h -> List.foldl ((++) << .inputs) [] h) signalHandlers) messages.signal

    views = Signal.map (List.map .view) signalHandlers

    -- result = Signal.foldp update ({html = text "initial", state = [], effects = Effects.none }) inputs
    result = Signal.map update inputs

    state = Signal.map fst result
    html  = Signal.map2 (\s vl -> List.foldr (\v parsed -> v address s) (text "initial") vl) state views
  in
    {
      html  = html
    , state = state
    , tasks = Signal.map (Effects.toTask messages.address << snd) result
    }

  -- case match matchers path of

mapHandler : Maybe Route -> Handler (List String)
mapHandler r = case r of
  Just Home               -> homeHandler
  Nothing                 -> homeHandler
  _                       -> homeHandler


homeHandler : Handler (List String)
homeHandler = {
    init = ["1"],
    view = (\sa s -> div [] [text <| "homeHandler: " ++ toString s]),
    inputs = [
      loadCategories
    ]
  }

loadCategories : Action (List String)
loadCategories state =
  let
    tsk = Http.get Json.Decode.string ("http://photo.awesomestuff.in/api/v1/category")
    tsk' = Task.andThen (Task.toMaybe tsk) (\r -> Task.succeed (updateCategories <| Maybe.withDefault "supgf" r))
  in DirtyState (state, Effects.task <| tsk')

updateCategories : String -> Action (List String)
updateCategories s state = DirtyState ([s], Effects.none)

type DirtyState state = DirtyState (state, Effects (Action state))

toState : DirtyState state -> (state, Effects (Action state))
toState (DirtyState s) = s

type alias Action state = state -> DirtyState state

type alias Handler state =
    { init    : state
    , view    : Signal.Address (Action state) -> state -> Html
    , inputs  : List (Action state)
    }

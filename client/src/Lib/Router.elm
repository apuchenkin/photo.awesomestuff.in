module Lib.Router (router, runRouter, initialState) where

import Dict
import Task         exposing (Task)
import Html         exposing (Html, text, div)
import Html.Events  exposing (onWithOptions)
import Effects      exposing (Effects, Never)
import History      exposing (path)

import Json.Decode as Json exposing ((:=))
import Html.Attributes exposing (href)
import Signal.Extra exposing (fairMerge, foldp')

import Lib.Helpers  exposing (..)
import Lib.Matcher  exposing (..)
import Lib.Types    exposing (..)
import Util.Util
-- import Response as R

initialState : RouterState route
initialState = {route = Nothing, params = Dict.empty}

mailbox : Signal.Mailbox (List (Action state))
mailbox = Signal.mailbox []

address : Signal.Address (Action state)
address = Signal.forwardTo mailbox.address singleton

-- Private: extract state from state
getState : WithRouter route state -> RouterState route
getState state = state.router

-- Private: set state in state
setState : WithRouter route state -> RouterState route -> WithRouter route state
setState state routerState =
  { state | router = routerState }

---------------------------------------------------------------------------

runHandlers : List (Handler state) -> List (Action state)
runHandlers handlers =
  let
    run actions state = Response <| List.foldl chainAction (noFx state) actions
  in List.map run <| List.map .inputs handlers


chainAction : Action state -> (state, ActionEffects state) -> (state, ActionEffects state)
chainAction action (state, effects) =
    let
        (Response (state', effects')) = action state
    in
        (state', Effects.batch [effects, effects'])

--------------------------------------------------------------------------------------
-- router functions
--------------------------------------------------------------------------------------

render : Router route (WithRouter route state) -> (WithRouter route state) -> Html
render (Router router) state =
    let
      _ = Debug.log "render" state
      (rs) = getState state
      route = rs.route
      handlers = Maybe.withDefault [] <| Maybe.map (router.getHandlers Nothing) route
      views =  List.map .view handlers
      html = List.foldr (\view parsed -> view address state parsed) Nothing views
    in Maybe.withDefault (text "error") html


setUrl : RouterConfig route (WithRouter route state) -> String -> Action (WithRouter route state)
setUrl config url = case (matchRoute config url) of
    Nothing               -> Debug.crash <| url
    Just route            -> setRoute config route


matchRoute : RouterConfig route state -> String -> Maybe (Route route)
matchRoute config url = List.head <| List.filterMap (flip (match config.config) url) <| config.routes


transition : RouterConfig route state -> Transition route state
transition config from to state =
  let
    _ = Debug.log "transition: from" from
    _ = Debug.log "transition: to" to
    _ = Debug.log "transition: state" state

    handlers = getHandlers config from to
    actions  = runHandlers handlers
  in  Response <| List.foldl chainAction (noFx state) actions

-- binds forward action to existing HTML attributes
bindForward : RouterConfig route (WithRouter route state) -> (WithRouter route state) -> Route route -> List Html.Attribute -> List Html.Attribute
bindForward config state route attrs =
  let
    options = {stopPropagation = True, preventDefault = True}
    action _ = Signal.message address <| forward config route
  in
    href (buildUrl config state route)
    :: onWithOptions "click" options Json.value action
    :: attrs

-- decompose Route to string
buildUrl : RouterConfig route (WithRouter route state) -> (WithRouter route state) -> Route route -> String
buildUrl config state route =
  let
    _ = Debug.log "buildUrl" (route, params)
    params = getState state |> .params
  in
    Lib.Matcher.buildUrl config.config config.routes <| combineParams params route

-- TODO: move abstract part to Matcher
getHandlers : RouterConfig route state -> Maybe route -> route -> List (Handler state)
getHandlers config from to =
  let
    _ = Debug.log "getHandlers" (from, to)
    zipperTo =   List.head <| List.filterMap (\r -> Util.Util.treeLookup to (r, [])) config.routes
    zipperFrom = List.head <| List.filterMap (\r -> from `Maybe.andThen` (flip Util.Util.treeLookup (r, []))) config.routes

    traverseTo   = Maybe.withDefault [] <| Maybe.map (Util.Util.traverseUp) zipperTo
    traverseFrom = Maybe.withDefault [] <| Maybe.map (Util.Util.traverseUp) zipperFrom

    routes = case traverseTo of
      [] -> []
      _  -> case (List.head traverseTo) == (List.head traverseFrom) of
        False -> []
        True  -> List.filter (\(f,t) -> f == t) <| List.map2 (,) traverseFrom traverseTo

    routes' = List.drop (List.length routes) traverseTo

  in List.map (.handler << config.config) routes'


setRoute : RouterConfig route (WithRouter route state) -> Route route -> Action (WithRouter route state)
setRoute config (route, params) state =
  let
    _ = Debug.log "setRoute" route
    (rs) = getState state
    from  = rs.route
    state' = setState state <| { rs | route = Just route, params = params }
  in
    transition config from route state'


forward : RouterConfig route (WithRouter route state) -> Route route -> Action (WithRouter route state)
forward config route state =
  let
    _ = Debug.log "forward" route
    url   = buildUrl config state route
    task  = History.setPath url |> Task.map (always (\s -> Response <| noFx s))
  in Response (state, Effects.task task)


router : RouterConfig route (WithRouter route state) -> Router route (WithRouter route state)
router config = Router {
    config        = config
  , bindForward   = bindForward   config
  , buildUrl      = buildUrl      config
  , getHandlers   = getHandlers   config
  , setRoute      = setRoute      config
  , forward       = forward       config
  }

runRouter : Router route (WithRouter route state) -> RouterResult (WithRouter route state)
runRouter (Router router) =
  let
    init = (Signal.map (singleton << (,) True << setUrl router.config) path)

    -- inputs : Signal (List (Bool, Action state))
    inputs =
      List.foldl (Signal.Extra.fairMerge List.append)
      (Signal.map (List.map ((,) False)) mailbox.signal) <| -- actions from events
      init
      :: List.map (Signal.map (singleton << (,) False)) router.config.inputs

    -- update : List (Bool, Action state) -> (state, ActionEffects state) -> (state, ActionEffects state)
    update  actions (state,_) = List.foldl chainAction (noFx state)
      <| List.map snd actions

    -- update' : List (Bool, Action state) -> (state, ActionEffects state)
    update' actions           = List.foldl chainAction (noFx router.config.init)
      <| List.map snd <| List.filter fst actions

    result = foldp' update update' inputs
    state = Signal.map fst result
  in
    {
      html  = Signal.map (render (Router router)) state
    , state = state
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }

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
import Lib.Matcher
import Lib.Types    exposing (..)
import Util.Util


-- import Response as R

initialState : RouterState route
initialState = {route = Nothing, params = Dict.empty, cache = {unwrap = Dict.empty, treeUrl = Dict.empty}}

mailbox : Signal.Mailbox (List (Action state))
mailbox = Signal.mailbox []

address : Signal.Address (Action state)
address = Signal.forwardTo mailbox.address singleton

---------------------------------------------------------------------------

runHandlers : List (Handler state) -> List (Action state)
runHandlers handlers =
  let
    run actions state = Response <| List.foldl runAction (noFx state) actions
  in List.map run <| List.map .inputs handlers

runAction : Action state -> (state, ActionEffects state) -> (state, ActionEffects state)
runAction action (state, effects) =
    let
        (Response (state', effects')) = action state
    in
        (state', Effects.batch [effects, effects'])

--------------------------------------------------------------------------------------
-- router functions
--------------------------------------------------------------------------------------

-- @private
render : Router route (WithRouter route state) -> (WithRouter route state) -> Html
render (Router router) state =
    let
      -- _ = Debug.log "render" state.router
      router'   = updateRouter (Router router) state.router
      route     = state.router.route
      handlers  = Maybe.withDefault [] <| Maybe.map (getHandlers router.config Nothing) route
      views     = flip List.map handlers <| (\handler -> (handler router').view)
      html      = List.foldr (\view parsed -> view address state parsed) Nothing views
    in Maybe.withDefault (text "error") html

-- @private
setUrl : Router route (WithRouter route state) -> RouterState route -> String -> Action (WithRouter route state)
setUrl (Router router) state url = case (matchRoute router.config state url) of
    Nothing               -> Debug.crash <| url
    Just route            -> setRoute (Router router) route

-- @private
setRoute : Router route (WithRouter route state) -> Route route -> Action (WithRouter route state)
setRoute router (route, params) state =
  let
    -- _ = Debug.log "setRoute" route
    rs = state.router
    from  = state.router.route
    state' = { state | router = { rs | route = Just route, params = params }}
  in
    transition router from route state'

-- @private
transition : Router route (WithRouter route state) -> Transition route (WithRouter route state)
transition (Router router) from to state =
  let
    -- _ = Debug.log "transition: from" from
    -- _ = Debug.log "transition: to" to
    -- _ = Debug.log "transition: state" state
    router'   = updateRouter (Router router) state.router
    handlers = getHandlers router.config from to
    actions  = runHandlers <| flip List.map handlers <| \handler -> handler router'
  in  Response <| List.foldl runAction (noFx state) actions

-- @private
matchRoute : RouterConfig route state -> RouterState route -> String -> Maybe (Route route)
matchRoute config state url =
  let
    rawRoute route = case Dict.get (.url <| config.config route) state.cache.unwrap of
      Just value -> value
      Nothing -> Lib.Matcher.unwrap <| .url <| config.config route
  in
    Lib.Matcher.matchRaw rawRoute config.routes url

-- @public
-- binds forward action to existing HTML attributes
bindForward : RouterConfig route (WithRouter route state) -> RouterState route -> Route route -> List Html.Attribute -> List Html.Attribute
bindForward config state route attrs =
  let
    options = {stopPropagation = True, preventDefault = True}
    action _ = Signal.message address <| forward config route
  in
    href (buildUrl config state route)
    :: onWithOptions "click" options Json.value action
    :: attrs

-- decompose Route to string
buildUrl : RouterConfig route (WithRouter route state) -> RouterState route -> Route route -> String
buildUrl config state (route, params) =
  let
  raw =  case Dict.get (toString route) state.cache.treeUrl of
    Just value -> value
    Nothing -> Lib.Matcher.buildTreeUrl (.url << config.config) config.routes route

  raws = case Dict.get raw state.cache.unwrap of
    Just value -> value
    Nothing -> Lib.Matcher.unwrap raw

  in Lib.Matcher.buildRawUrl raws <| Lib.Matcher.combineParams state.params (route, params)

-- TODO: cache
getHandlers : RouterConfig route state -> Maybe route -> route -> List (Router route state -> Handler state)
getHandlers config from to =
  let routes = case from == Just to of
    True -> [to]
    False ->
      let
        lca = from `Maybe.andThen` \from' -> Util.Util.lca config.routes from' to
        zipperTo = List.head <| List.filterMap (\r -> Util.Util.treeLookup to (r, [])) config.routes
      in Maybe.withDefault []
        <| flip Maybe.map zipperTo
        <| \z -> Util.Util.traverseFrom z lca

  in List.map (.handler << config.config) <| routes


forward : RouterConfig route (WithRouter route state) -> Route route -> Action (WithRouter route state)
forward config route state =
  let
    _ = Debug.log "forward" route
    url   = buildUrl config state.router route
    task  = History.setPath url |> Task.map (always (\s -> Response <| noFx s))
  in Response (state, Effects.task task)

updateRouter : Router route (WithRouter route state) -> RouterState route -> Router route (WithRouter route state)
updateRouter (Router router) rs = Router { router |
    getRoute    = rs.route,
    getParams   = rs.params,
    buildUrl    = buildUrl router.config rs,
    bindForward = bindForward router.config rs
  }

router : RouterConfig route (WithRouter route state) -> Router route (WithRouter route state)
router config = let routerState = config.init.router
  in Router {
    config        = config
  , getRoute      = routerState.route
  , getParams     = routerState.params
  , bindForward   = bindForward   config routerState
  , buildUrl      = buildUrl      config routerState
  , forward       = forward       config
  }

prepareCache : (WithRouter route state) -> RouterConfig route (WithRouter route state) -> (WithRouter route state)
prepareCache state config =
  let
    router = state.router
    routes = List.concat <| List.map Util.Util.treeToList config.routes
    urls = flip List.map routes <| \r -> (toString r, Lib.Matcher.buildTreeUrl (.url << config.config) config.routes r)
    urls' = List.map (.url << config.config) routes
    unwraps = flip List.map (urls' ++ List.map snd urls) <| \url -> (url, Lib.Matcher.unwrap url)
    treeUrl = Dict.fromList urls
    unwrap  = Dict.fromList unwraps
    cache = {treeUrl = treeUrl, unwrap = unwrap}
  in {state | router = {router | cache = cache}}

runRouter : Router route (WithRouter route state) -> RouterResult (WithRouter route state)
runRouter (Router router) =
  let
    initialState = prepareCache router.config.init router.config
    -- _ = Debug.log "initialState" initialState
    init = (Signal.map (singleton << (,) True << setUrl (Router router) initialState.router) path)

    -- inputs : Signal (List (Bool, Action state))
    inputs =
      List.foldl (Signal.Extra.fairMerge List.append)
      init <|
      (Signal.map (List.map ((,) False)) mailbox.signal) -- actions from events
      :: List.map (Signal.map (singleton << (,) False)) router.config.inputs

    -- update : List (Bool, Action state) -> (state, ActionEffects state) -> (state, ActionEffects state)
    update  actions (state,_) = List.foldl runAction (noFx state)
      <| List.map snd actions

    -- update' : List (Bool, Action state) -> (state, ActionEffects state)
    update' actions           = List.foldl runAction (noFx initialState)
      <| List.map snd
      <| List.filter fst actions

    result = foldp' update update' inputs
    state = Signal.map fst result
  in
    {
      html  = Signal.map (render (Router router)) state
    , state = state
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }

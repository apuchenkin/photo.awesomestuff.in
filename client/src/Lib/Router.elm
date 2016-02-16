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
import MultiwayTreeUtil


-- import Response as R

initialState : RouterState route
initialState = {
    route = Nothing
  , params = Dict.empty
  , cache = {unwrap = Dict.empty, rawUrl = Dict.empty, routePath = Dict.empty}
  }

mailbox : Signal.Mailbox (List (Action state))
mailbox = Signal.mailbox []

address : Signal.Address (Action state)
address = Signal.forwardTo mailbox.address singleton

---------------------------------------------------------------------------

runHandlers : List (Handler state) -> List (Action state)
runHandlers handlers =
  let
    run actions state = Response <| List.foldl runAction (noFx state) actions
  in List.map run <| List.map .actions handlers

runAction : Action state -> (state, ActionEffects state) -> (state, ActionEffects state)
runAction action (state, effects) =
    let
        (Response (state', effects')) = action state
    in
        (state', Effects.batch [effects, effects'])

--------------------------------------------------------------------------------------
-- private router functions
--------------------------------------------------------------------------------------

{-| @Private -}
prepareCache : (WithRouter route state) -> RouterConfig route (WithRouter route state) -> (WithRouter route state)
prepareCache state config =
  let
    router = state.router
    routes = List.concat <| List.map MultiwayTreeUtil.flatten config.routes
    urls = flip List.map routes <| \r -> (toString r, Lib.Matcher.composeRawUrl (.segment << config.config) config.routes r)
    urls' = List.map (.segment << config.config) routes
    unwraps = flip List.map (urls' ++ List.map snd urls) <| \url -> (url, Lib.Matcher.unwrap url)
    rawUrl = Dict.fromList urls
    unwrap  = Dict.fromList unwraps
    routePaths = List.foldl (\from acc -> acc ++ List.map (\to -> ((Maybe.withDefault "" <| Maybe.map toString from, toString to), getPath from to config.routes)) routes) [] (Nothing :: List.map Just routes)
    routePath = Dict.fromList routePaths
    cache = {rawUrl = rawUrl, unwrap = unwrap, routePath = routePath}
  in {state | router = {router | cache = cache}}

{-| @Private -}
render : Router route (WithRouter route state) -> (WithRouter route state) -> Html
render router state =
    let
      -- _ = Debug.log "render" state.router
      route     = state.router.route
      handlers  = Maybe.withDefault [] <| Maybe.map (\to -> getHandlers router state.router Nothing (to, Dict.empty)) route
      views     = List.map .view handlers
      html      = List.foldr (\view parsed -> view address state parsed) Nothing views
    in Maybe.withDefault (text "error") html

{-| @Private -}
setUrl : Router route (WithRouter route state) -> RouterState route -> String -> Action (WithRouter route state)
setUrl router state url =
  let
  _ = Debug.log "setUrl" url
  (Router r) = router
  in case (matchRoute r.config state url) of
    Nothing               -> setRoute router r.config.fallback
    Just route            -> setRoute router route

{-| @Private -}
setRoute : Router route (WithRouter route state) -> Route route -> Action (WithRouter route state)
setRoute router route state =
  let
    -- _ = Debug.log "setRoute" route
    rs = state.router
    (toRoute, toParams) = route
    from  = Maybe.map (\r -> (r, rs.params)) rs.route
    state' = { state | router = { rs | route = Just toRoute, params = toParams }}
  in
    transition router from route state'

{-| @Private -}
transition : Router route (WithRouter route state) -> Transition route (WithRouter route state)
transition router from to state =
  let
    -- _ = Debug.log "transition: from" (from, to)
    handlers = getHandlers router state.router from to
    actions  = runHandlers handlers
  in  Response <| List.foldl runAction (noFx state) actions

{-| @Private -}
getHandlers : Router route state -> RouterState route -> Maybe (Route route) -> Route route -> List (Handler state)
getHandlers router state from to =
  let
    (Router r) = router
    fromRoute = Maybe.map fst from
    toRoute = fst to
    routes = case fromRoute == Just toRoute of
      True  -> [toRoute]
      False -> case Dict.get (Maybe.withDefault "" <| Maybe.map toString fromRoute, toString toRoute) state.cache.routePath of
        Just path -> path
        Nothing   -> getPath fromRoute toRoute r.config.routes

  in List.map ((\h -> h router) << .handler << r.config.config) <| routes

{-| @Private -}
matchRoute : RouterConfig route state -> RouterState route -> String -> Maybe (Route route)
matchRoute config state url =
  let
    rawRoute route = case Dict.get (.segment <| config.config route) state.cache.unwrap of
      Just value -> (value, .constraints <| config.config route)
      Nothing -> (Lib.Matcher.unwrap <| .segment <| config.config route, .constraints <| config.config route)
  in
    Lib.Matcher.matchRaw rawRoute config.routes url

--------------------------------------------------------------------------------------
-- public router functions
--------------------------------------------------------------------------------------

-- @public
-- binds forward action to existing HTML attributes
bindForward : RouterConfig route (WithRouter route state) -> RouterCache route -> Route route -> List Html.Attribute -> List Html.Attribute
bindForward config cache route attrs =
  let
    options = {stopPropagation = True, preventDefault = True}
    action _ = Signal.message address <| forward config route
  in
    href (buildUrl config cache route)
    :: onWithOptions "click" options Json.value action
    :: attrs

-- decompose Route to string
buildUrl : RouterConfig route (WithRouter route state) -> RouterCache route -> Route route -> String
buildUrl config cache (route, params) =
  let
  raw =  case Dict.get (toString route) cache.rawUrl of
    Just value -> value
    Nothing -> Lib.Matcher.composeRawUrl (.segment << config.config) config.routes route

  raws = case Dict.get raw cache.unwrap of
    Just value -> value
    Nothing -> Lib.Matcher.unwrap raw

  in Lib.Matcher.buildRawUrl raws (route, params) -- Lib.Matcher.combineParams state.params

forward : RouterConfig route (WithRouter route state) -> Route route -> Action (WithRouter route state)
forward config route state =
  let
    _ = Debug.log "forward" route
    url   = buildUrl config state.router.cache route
    task  = History.setPath url |> Task.map (always (\s -> Response <| noFx s))
  in Response (state, Effects.task task)

{-| Router constructor -}
router : RouterConfig route (WithRouter route state) -> Router route (WithRouter route state)
router config =
  let
    state = if config.useCache
      then prepareCache config.init config
      else config.init

  in Router {
    config        = {config | init = state}
  , bindForward   = bindForward   config state.router.cache
  , buildUrl      = buildUrl      config state.router.cache
  , forward       = forward       config
  }

runRouter : Router route (WithRouter route state) -> RouterResult (WithRouter route state)
runRouter router =
  let
    -- _ = Debug.log "initialState" initialState
    (Router r) = router
    initialState = r.config.init
    init = (Signal.map (singleton << (,) True << setUrl router initialState.router) path)

    -- inputs : Signal (List (Bool, Action state))
    inputs =
      List.foldl (Signal.Extra.fairMerge List.append)
      init <|
      (Signal.map (List.map ((,) False)) mailbox.signal) -- actions from events
      :: List.map (Signal.map (singleton << (,) True))  r.config.inits
      ++ List.map (Signal.map (singleton << (,) False)) r.config.inputs

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
      html  = Signal.map (render router) state
    , state = state
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }

module Lib.Router where

import Task         exposing (Task)
import RouteParser  exposing (Matcher, match, mapMatcher)
import Html         exposing (Html, text, div)
import Html.Events  exposing (onWithOptions)
import Effects      exposing (Effects, Never)
import History      exposing (path)


import Json.Decode as Json exposing ((:=))
import Html.Attributes exposing (href)
import Signal.Extra exposing (fairMerge, foldp')
-- import Response as R

-- definitions
-----------------------------------------
-- State
-----------------------------------------

type alias Action state = state -> Response state
type alias ActionEffects state = Effects (Action state)
-- type alias MaybeTask state = Maybe (Task Never (Action state))

type alias Result state =
    { html  : Signal Html
    , state : Signal state
    -- , tasks : Signal (Task.Task Never (List ()))
    , tasks : Signal (Task Never ())
    }

type alias Handler state = {
    view    : Signal.Address (Action state) -> state -> Maybe Html -> Maybe Html
  , inputs  : List (Action state)
  }

type Response state = Response (state, ActionEffects state)

type RouterState route = RouterState {
    route:    Maybe route,
    params:   List String
  }

-----------------------------------------
-- Route
-----------------------------------------

{-| Type extension for the model. -}
type alias WithRouter route state = { state | router : RouterState route }

type alias RouteConfig route state = {
      parent:       Maybe route
    , url:          String
    , buildUrl:     String
    , matcher:      Matcher route
    , handler:      List (Handler state)
  }

type alias RouterConfig route state = {
  init:       state,
  config:     route -> RouteConfig route state,
  routes:     List (route),
  inputs:     List (Signal.Signal (Action state))
}

type Router route state = Router {
  config        : RouterConfig route state,
  state         : RouterState route,
  matchRoute    : String -> Maybe route,
  bindForward   : route -> List Html.Attribute -> List Html.Attribute,
  buildUrl      : route -> String,
  getHandlers   : Maybe route -> route -> List (Handler state),
  setRoute      : route -> Action state,
  forward       : route -> Action state
}

type alias Transition route state = Maybe route -> route -> Action state

--------------------------------------------------------------------------------
noFx : state -> (state, ActionEffects state)
noFx state = (state, Effects.none)

initialState : RouterState route
initialState = RouterState {route = Nothing, params = []}

-- Private: extract state from state
getState : WithRouter route state -> RouterState route
getState state = state.router
  -- case state.router of
  --   RouterState router -> router

-- Private: set state in state
setState : WithRouter route state -> RouterState route -> WithRouter route state
setState state routerState =
  { state | router = routerState }


router : RouterConfig route (WithRouter route state) -> Router route (WithRouter route state)
router config =
  let
    -- matchRoute : String -> Maybe route
    matchRoute url =
      let matchers = List.map (.matcher << config.config) config.routes
      in match matchers url

    -- decompose Route to string
    buildUrl route =
      let url = .buildUrl <| config.config route
      in url

    -- buildMatcher : route -> Matcher route
    -- buildMatcher route =
    --   let matcher = .matcher <| config.config route
    --   in  matcher

    -- binds forward action to existing HTML attributes
    -- bindForward : route -> List Html.Attribute -> List Html.Attribute

    -- getHandlers : Maybe route -> route -> List (Handler state)
    getHandlers from to = (.handler << config.config) to

    -- setRoute : route -> Action (WithRouter route state)
    setRoute route state =
      let
        _ = Debug.log "setRoute" route
        (RouterState rs) = getState state
        from  = rs.route
        state' = setState state <| RouterState { rs | route = Just route }
      in
        transition from route state'

    -- transition : Transition route state
    transition from to state =
      let
        _ = Debug.log "transition: from" from
        _ = Debug.log "transition: to" to
        _ = Debug.log "transition: state" state

        signalHandlers = getHandlers from to
        actions = runHandlers signalHandlers
      in  Response <| List.foldl chainAction (noFx state) actions

    bindForward route attrs =
      let
        options = {stopPropagation = True, preventDefault = True}
        action _ = Signal.message address <| forward route
      in
           onWithOptions "click" options Json.value action
        :: href (buildUrl route)
        :: attrs

    -- forward : route -> state -> Action (WithRouter route state)
    forward route state =
      let
        _ = Debug.log "forward" route
        task  = History.setPath (buildUrl route) |> Task.map (\_ -> setRoute route)
      in Response (state, Effects.task task)

  in Router {
    config        = config
  , matchRoute    = matchRoute
  , state         = initialState
  , bindForward   = bindForward
  , buildUrl      = buildUrl
  , getHandlers   = getHandlers
  , setRoute      = setRoute
  -- , buildMatcher  = buildMatcher
  , forward       = forward
  }

singleton : a -> List a
singleton action = [ action ]

mailbox : Signal.Mailbox (List (Action state))
mailbox = Signal.mailbox []

mailbox' : Signal.Mailbox (Action state)
mailbox' = Signal.mailbox (\state -> Response <| noFx state)

address : Signal.Address (Action state)
address = Signal.forwardTo mailbox.address singleton

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

render : Router route (WithRouter route state) -> (WithRouter route state) -> Html
render (Router router) state =
    let
      _ = Debug.log "render" state
      (RouterState rs) = getState state
      route = rs.route
      handlers = Maybe.withDefault [] <| Maybe.map (router.getHandlers Nothing) route
      views =  List.map .view handlers
      html = List.foldr (\view parsed -> view address state parsed) Nothing views
    in Maybe.withDefault (text "error") html


setUrl : Router route (WithRouter route state) -> String -> Action (WithRouter route state)
setUrl (Router router) url = case (router.matchRoute url) of
    Nothing    -> Debug.crash <| url
    Just route -> router.setRoute route

runRouter : Router route (WithRouter route state) -> Result (WithRouter route state)
runRouter (Router router) =
  let
    init =
      Signal.merge
      (Signal.map (setUrl (Router router)) path)
      mailbox'.signal

    inputs =
      List.foldl (Signal.Extra.fairMerge List.append)
      mailbox.signal <| -- actions from events
      List.map (Signal.map singleton) router.config.inputs

    update  actions (state,_) = List.foldl chainAction (noFx state)              <| snd actions
    update' action            = let (Response s) = (fst action) router.config.init in s

    result = foldp' update update' <| Signal.Extra.zip init inputs
    state = Signal.map fst result
  in
    {
      html  = Signal.map (render (Router router)) state
    , state = state
    -- , tasks = Signal.map (\lt -> Task.sequence <| List.map (Effects.toTask mailbox.address) (snd lt)) result
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }

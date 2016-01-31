module Lib.Router where

import Task
import String
import Array
import RouteParser exposing (Matcher, match, mapMatcher)
import Html     exposing (Html, text, div)
import Html.Events exposing (onWithOptions)
import Effects  exposing (Effects, Never)
import History  exposing (path)
-- import RouteParser
import MultiwayTree as Tree exposing (Tree (..))
import Json.Decode as Json exposing ((:=))
import Html.Attributes exposing (href)
import Signal.Extra exposing (fairMerge, foldp')
import Response as R

-- definitions
type alias Result state =
    { html  : Signal Html
    , state : Signal state
    , tasks : Signal (Task.Task Never ())
    }

type RouterState route = RouterState {
    route:    Maybe route,
    params:   List String
  }

{-| Type extension for the model. -}
type alias WithRouter route state = { state | router : RouterState route }

type alias RouteConfig route state = {
      url:          String
    , buildUrl:     String
    , matcher:      Matcher route
    , handler:      Handler state
  }

type alias RouterConfig route state = {
  init:       state,
  config:     route -> RouteConfig route state,
  routes:     Tree route,
  inputs:     List (Signal.Signal (Action state))
}

type Router route state = Router {
  config        : RouterConfig route state,
  state         : RouterState route,
  matchRoute    : String -> Maybe route,
  bindForward   : route -> List Html.Attribute -> List Html.Attribute,
  buildUrl      : route -> String,
  buildMatcher  : route -> Matcher route,
  forward       : route -> Action state
}

type Response state = Response (R.Response state (Action state))

-- type DirtyState route state = DirtyState ((WithRouter route state), Effects (Action route (WithRouter route state)))

type alias Action state = state -> Response state

type alias Handler state = {
    view    : Signal.Address (Action state) -> state -> Maybe Html -> Maybe Html
  , inputs  : List (Action state)
  }

type alias Transition route state = Maybe route -> route -> Action state

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

setRoute : route -> Action (WithRouter route state)
setRoute route state =
  let
    (RouterState rs) = getState state
    from  = rs.route
    state' = setState state <| RouterState { rs | route = Just route }
  in
    transition from route state'

transition : Transition route state
transition from to = (\state -> Response (state, Effects.none))

router : RouterConfig route state -> Router route state
router config =
  let
    -- matchRoute : String -> Maybe route
    matchRoute url = match (List.map (Tree.datum) [Tree.map buildMatcher config.routes]) url

    -- decompose Route to string
    buildUrl route =
      let url = .buildUrl <| config.config route
      in url

    -- TODO: Nesting
    -- buildMatcher : route -> Matcher route
    buildMatcher route =
      let matcher = .matcher <| config.config route
      in  matcher

    -- binds forward action to existing HTML attributes
    -- bindForward : route -> List Html.Attribute -> List Html.Attribute
    bindForward route attrs =
      let
        options = {stopPropagation = True, preventDefault = True}
        action _ = Signal.message address <| forward route
      in
           onWithOptions "click" options Json.value action
        :: href (buildUrl route)
        :: attrs

    -- forward : route -> Action route state
    forward route state =
      let
        tsk  = History.setPath <| buildUrl route
        tsk' = Task.andThen (Task.toMaybe tsk) (\r -> Task.succeed (\a -> Response (a, Effects.none)))
      in Response (state, Effects.task <| tsk')

  in Router {
    config        = config
  , matchRoute    = matchRoute
  , state         = initialState
  , bindForward   = bindForward
  , buildUrl      = buildUrl
  , buildMatcher  = buildMatcher
  , forward       = forward
  }



pathHandlers : Router route state -> String -> List (Handler state)
pathHandlers (Router router) url =
  let
    mapSegment s (forest, acc) =
      let
        maybeRoute = match (List.indexedMap (\i v -> mapMatcher (\m -> (i,m)) (Tree.datum v)) forest) s
        forest' = Maybe.withDefault [] <| Maybe.andThen maybeRoute (\m -> Maybe.map Tree.children (Array.get (fst m) (Array.fromList forest)))
        acc' = acc ++ [Maybe.map snd maybeRoute]
      in (forest', acc')

    maybeRoutes = snd
        <| List.foldl mapSegment ([Tree.map router.buildMatcher router.config.routes], [])
        <| String.split "/" url

    handlers = List.filterMap (\r -> Maybe.map (.handler << router.config.config) r) maybeRoutes

  in handlers

singleton : a -> List a
singleton action = [ action ]

mailbox : Signal.Mailbox (List (Action state))
mailbox = Signal.mailbox []

address : Signal.Address (Action state)
address = Signal.forwardTo mailbox.address singleton

runHandlers : List (Handler state) -> Action state
runHandlers handlers =
  let
    runAction action (state, effects) =
        let
            (Response (state', effects')) = action state
            -- task = Effects.toTask mailbox.address effects
            -- effects'' = Effects.task <| Task.andThen task (\_ -> Task.succeed action)
        in
            -- (state', effects'')
            (state', effects')

    update actions ds = List.foldl (\a (s,e) -> runAction a (s,e)) ds actions
    -- result =

    -- inputs = Signal.mergeMany <|
    --      Signal.map (\h -> List.foldl ((++) << .inputs) [] h) handlers
    --   :: mailbox.signal
    --   :: []

      -- :: List.map (Signal.map singleton) config.inputs

    -- update' actions (s,_) = List.foldl updateStep ([], Effects.none) actions
    -- result = Signal.foldp update' (["f"], Effects.none) inputs
    -- result = Signal.map update inputs
  in (\initial -> Response <| List.foldl update (initial, Effects.none) <| List.map .inputs handlers)

runRouter : Router route (WithRouter route state) -> Result (WithRouter route state)
runRouter (Router router) =
  let
    -- setRouter router = router = router
    -- setRoute route =
    -- let
    --   state = getState model
    --   prevRoute = state.route
    --   newModel = setState model { state | route = route }
    -- in
    --   config.mountRoute prevRoute route newModel

    act = Signal.map (\p -> case (router.matchRoute p) of
        Nothing    -> Debug.crash <| toString p
        Just route -> setRoute route
      ) path

    -- signalHandlers = Signal.map (pathHandlers router) path
    -- transitionAction = Signal.map runHandlers signalHandlers
    --
    inputs = Signal.mergeMany <|
         Signal.map singleton act
      :: mailbox.signal
      :: List.map (Signal.map singleton) router.config.inputs
      -- :: []
    --
    runAction action (state, effects) =
        let
            (Response (state', effects')) = action state
        in
            (state', Effects.batch [effects, effects'])
    --
    update actions ds = List.foldl runAction ds actions
    update' actions (s,_) = List.foldl runAction (s, Effects.none) actions
    update'' actions = List.foldl runAction (router.config.init, Effects.none) actions
    result = foldp' update' (\a -> update a (router.config.init,Effects.none)) inputs
    -- result = Signal.foldp update' (router.config.init, Effects.none) inputs

    -- result = Signal.map update'' inputs
    state = Signal.map fst result
    -- views = Signal.map (List.map .view) signalHandlers
    -- html  = Signal.map2 (\state viewList -> Maybe.withDefault (text "error") <| List.foldr (\view parsed -> view address state parsed) Nothing viewList) state views
  in
    {
      html  = Signal.map (text << toString) state
    , state = state
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }

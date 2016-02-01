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
  -- buildMatcher  : route -> Matcher route,
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

noFx : state -> (state, Effects a)
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

setRoute : Router route (WithRouter route state) -> route -> Action (WithRouter route state)
setRoute router route state =
  let
    _ = Debug.log "setRoute" route
    (RouterState rs) = getState state
    from  = rs.route
    state' = setState state <| RouterState { rs | route = Just route }
  in
    transition router from route state'

transition : Router route state -> Transition route state
transition router from to state =
  let
    _ = Debug.log "transition: from" from
    _ = Debug.log "transition: to" to
    _ = Debug.log "transition: state" state

    signalHandlers = routeHandlers router from to

    -- action state = Response (state, Effects.none)
    -- task = Task.succeed <| action
  in
    runHandlers signalHandlers state

router : RouterConfig route state -> Router route state
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
    bindForward route attrs =
      let
        _ = Debug.crash "bindForward"
        options = {stopPropagation = True, preventDefault = True}
        action _ = Signal.message address <| forward route
      in
           onWithOptions "click" options Json.value action
        :: href (buildUrl route)
        :: attrs

    -- forward : route -> Action route state
    forward route state =
      let
        _ = Debug.crash "forward"
        tsk  = History.setPath <| buildUrl route
        tsk' = Task.andThen (Task.toMaybe tsk) (\r -> Task.succeed <| Response << noFx)
      in Response (state, Effects.task <| tsk')

  in Router {
    config        = config
  , matchRoute    = matchRoute
  , state         = initialState
  , bindForward   = bindForward
  , buildUrl      = buildUrl
  -- , buildMatcher  = buildMatcher
  , forward       = forward
  }

-- routeChain : Router route state -> Maybe route -> route -> List (route)
-- routeChain (Router router) from to =
--   let
--     config = router.config.config to
--     result = [to]
--   in case config.parent of
--     Nothing    -> result
--     Just route -> case from of
--       Nothing -> (routeChain (Router router) from route) ++ result
--       Just f  -> case route == f of
--         True  -> route :: result
--         False -> (routeChain (Router router) from route) ++ result

routeHandlers : Router route state -> Maybe route -> route -> List (Handler state)
routeHandlers (Router router) from to = (.handler << router.config.config) to
  -- let chain = routeChain (Router router) from to
  -- in List.map (.handler << router.config.config) chain

singleton : a -> List a
singleton action = [ action ]

mailbox : Signal.Mailbox (List (Action state))
mailbox = Signal.mailbox []

address : Signal.Address (Action state)
address = Signal.forwardTo mailbox.address singleton

chainActions : List (Action state) -> Action state
chainActions actions state =
  let
    -- first = List.head actions
    -- response = case first of
    --   Nothing -> Response (noFx state)
    --   Just action ->
    --     let tail = List.tail actions
    --     case tail of
    --       Nothing -> action state
    --       Just actions' ->
    --         Response <| List.foldl
    --         let task = Task.succeed action


    runAction action (state, effects) =
        let
            task = Effects.toTask mailbox.address effects
            effects'' = Effects.task <| Task.andThen task (\act -> Task.succeed action)
            -- (Response (state', effects')) = action state
        in
            (state, effects'')

    -- update actions state = Response <| List.foldl runAction (noFx state) actions
    -- run action (s, e) = Task.succeed action `Task.andThen`
    -- action = List.foldl run (noFx state) actions
    -- tasks   = List.map Task.succeed actions
    -- effects = Effects.task <| Task.sequence tasks
    -- effects' = Effects.map (\actions s -> ) effects

  in Response <| List.foldl runAction (noFx state) actions

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
            (state', Effects.batch [effects, effects'])

    update actions state = Response <| List.foldl runAction (noFx state) actions
    actions = List.map (update << .inputs) handlers

    _ = Debug.log "runHandlers" <| toString handlers
    -- result =

    -- inputs = Signal.mergeMany <|
    --      Signal.map (\h -> List.foldl ((++) << .inputs) [] h) handlers
    --   :: mailbox.signal
    --   :: []

      -- :: List.map (Signal.map singleton) config.inputs

    -- update' actions (s,_) = List.foldl updateStep ([], Effects.none) actions
    -- result = Signal.foldp update' (["f"], Effects.none) inputs
    -- result = Signal.map update inputs
  -- in (\initial -> Response <| List.foldl update (noFx initial) <| List.map .inputs handlers)
  in chainActions actions

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
        Just route -> setRoute (Router router) route
      ) path


    -- _ = Debug.log "handlers" <| toString signalHandlers
    --
    inputs = Signal.mergeMany <|
         mailbox.signal
      :: List.map (Signal.map singleton) router.config.inputs

    sig = Signal.map2 (,) (Signal.map singleton act) inputs
      -- :: []
    --
    runAction action (state, effects) =
        let
            (Response (state', effects')) = action state
        in
            (state', Effects.batch [effects, effects'])
    --
    update actions (s,_) = List.foldl runAction (noFx s) <| snd actions
    update' actions = List.foldl runAction (noFx router.config.init) <| fst actions
    -- update'' actions = List.foldl runAction (router.config.init, Effects.none) actions
    result = foldp' update update' sig
    -- result = Signal.foldp update' (router.config.init, Effects.none) inputs

    -- result = Signal.map update'' inputs
    state = Signal.map fst result

    -- st = Signal.map getState state
    -- signalHandlers = Signal.map (\(RouterState s) -> pathHandlers (Router router) <| .route s) st
    -- views = Signal.map (List.map .view) signalHandlers
    -- html  = Signal.map2 (\state viewList -> Maybe.withDefault (text "error") <| List.foldr (\view parsed -> view address state parsed) Nothing viewList) state views
  in
    {
      html  = Signal.map (text << toString) state
    , state = state
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }

module Lib.Router where

import Task
import String
import Array
import RouteParser as R exposing (Matcher, match, mapMatcher, static)
import Html     exposing (Html, text, div)
import Html.Events exposing (onWithOptions)
import Effects  exposing (Effects, Never)
import History  exposing (path)
import RouteParser
import MultiwayTree as Tree exposing (Tree (..))
import Json.Decode as Json exposing ((:=))
import Html.Attributes exposing (href)
import Signal.Extra exposing (fairMerge, foldp')

-- definitions
type alias Result state =
    { html  : Signal Html
    , state : Signal state
    , tasks : Signal (Task.Task Never ())
    }

type alias RouterState route = {
    route:    Maybe route,
    params:   List String
  }

type alias RouteConfig state route = {
      url:          String
    , buildUrl:     String
    , matcher:      Matcher route
    , handler:      Handler state
  }

type alias RouterConfig state route = {
  init:       state,
  config:     route -> RouteConfig state route,
  routes:     Tree route,
  inputs:     List (Signal.Signal (Action state))
}

type alias Router state route = {
  config        : RouterConfig state route,
  state         : RouterState route,
  bindForward   : route -> List Html.Attribute -> List Html.Attribute,
  buildUrl      : route -> String,
  buildMatcher  : route -> Matcher route,
  forward       : route -> Action state
}

type DirtyState state = DirtyState (state, Effects (Action state))

type alias Action state = state -> DirtyState state

type alias Handler state = {
    view    : Signal.Address (Action state) -> state -> Maybe Html -> Maybe Html
  , inputs  : List (Action state)
  }

type alias Transition state route = route -> route -> Action state

initialState : RouterState route
initialState = {route = Nothing, params = []}

router : RouterConfig state route -> Router state route
router config =
  let
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

    -- forward : route -> Action state
    forward route state =
      let
        tsk  = History.setPath <| buildUrl route
        tsk' = Task.andThen (Task.toMaybe tsk) (\r -> Task.succeed (\a -> DirtyState (a, Effects.none)))
      in DirtyState (state, Effects.task <| tsk')
  in {
    config        = config
  , state         = initialState
  , bindForward   = bindForward
  , buildUrl      = buildUrl
  , buildMatcher  = buildMatcher
  , forward       = forward
  }

pathHandlers : Router state route -> String -> List (Handler state)
pathHandlers router url =
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
            (DirtyState (state', effects')) = action state
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
  in (\initial -> DirtyState <| List.foldl update (initial, Effects.none) <| List.map .inputs handlers)

runRouter : Router state route -> Result state
runRouter router =
  let
    signalHandlers = Signal.map (pathHandlers router) path
    transitionAction = Signal.map runHandlers signalHandlers

    inputs = Signal.mergeMany <|
         Signal.map singleton transitionAction
      :: mailbox.signal
      :: List.map (Signal.map singleton) router.config.inputs
      -- :: []

    runAction action (state, effects) =
        let
            (DirtyState (state', effects')) = action state
        in
            (state', Effects.batch [effects, effects'])

    update actions ds = List.foldl runAction ds actions
    update' actions (s,_) = List.foldl runAction (s, Effects.none) actions
    result = foldp' update (\a -> update a (router.config.init,Effects.none)) inputs
    -- result = Signal.foldp update' (router.config.init,Effects.none) inputs

    state = Signal.map fst result
    views = Signal.map (List.map .view) signalHandlers
    html  = Signal.map2 (\state viewList -> Maybe.withDefault (text "error") <| List.foldr (\view parsed -> view address state parsed) Nothing viewList) state views
  in
    {
      html  = html
    , state = state
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }

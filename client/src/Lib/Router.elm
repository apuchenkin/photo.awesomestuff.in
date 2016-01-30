module Lib.Router where

import Task
import String
import Array
import Regex
import RouteParser as R exposing (Matcher, match, mapMatcher, static)
import Html     exposing (Html, text, div)
import Html.Events exposing (onWithOptions)
import Effects  exposing (Effects, Never)
import History  exposing (path)
import RouteParser
import MultiwayTree as Tree exposing (Tree (..))
import Json.Decode as Json exposing ((:=))
import Html.Attributes exposing (href)
import List.Extra as L exposing ((!!))


-- definitions

type alias Result state =
    { html  : Signal Html
    , state : Signal state
    , tasks : Signal (Task.Task Never ())
    }

type alias RouterState = {
    state:  Maybe String,
    handlers: List (Handler (List String))
  }

type alias RouteConfig state route = {
      url:          String
    , constructor:  List String -> route
    , params:       List (String, String)
    , handler:      Handler state
  }

type alias RouterConfig state route = {
  init:       state,
  config:     Config state route,
  routes:     Tree route,
  inputs:     List (Signal.Signal (Action state))
}

type alias Router state route = {
  config        : RouterConfig state route,
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

-- type alias HandlerMap state route = route -> Handler state
type alias Config state route = route -> RouteConfig state route

router : RouterConfig state route -> Router state route
router config =
  let
    -- decompose Route to string
    buildUrl route =
      let
        cfg = config.config route
        url = List.foldl (\p string -> Regex.replace Regex.All (Regex.regex <| "#" ++ fst p) (\_ -> snd p) string) cfg.url cfg.params
      in url


    -- TODO: R2, R3, abstract, Optional params support
    -- buildMatcher : route -> Matcher route
    buildMatcher route =
      let
        isStatic sergment = String.startsWith "#" sergment
        cfg = config.config route
        url = cfg.url
        segments  = String.split "/" url
        segments' = List.filterMap (\s -> case isStatic s of
            True  -> Maybe.map (\r -> (True, snd r)) <| String.uncons s
            False -> Just (False, s)
          ) segments
        -- _ = Debug.log "s" <| toString segments'
        dynCount = List.length <| List.filter fst segments'
        -- groups = L.groupBy (\a b -> fst a == fst b) <| (False,"") :: segments' ++ [(False,"")]
        -- segments'' = List.map (\g -> List.foldl (\(i,s) acc -> acc ++ s) "" g) groups
        strings = Regex.split Regex.All (Regex.regex "#[^/]+") url
        -- _ = Debug.log "test:" <| toString <|
        segments'' = L.interweave (List.map (\s -> (False,s)) strings) (List.filter fst segments')
        _ = Debug.log "test:" <| toString <| segments''

        seg : Int -> String
        seg idx = Maybe.withDefault "" <| Maybe.map snd <| segments'' !! idx

        -- matcher : R.Matcher route
        matcher = case dynCount of
          0 -> static route url
          1 -> R.dyn1 (\a -> cfg.constructor [a]) (seg 0) R.string (seg 2)
          -- 2 ->
          -- 3 ->
          _ -> static route url
      in
        matcher

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
  , bindForward   = bindForward
  , buildUrl      = buildUrl
  , buildMatcher  = buildMatcher
  , forward       = forward
  }


toState : DirtyState state -> (state, Effects (Action state))
toState (DirtyState s) = s

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

runHandlers : state -> Signal (List (Handler state)) -> Signal (state, Effects (Action state))
runHandlers initial handlers =
  let
    updateStep action (state, effects) =
        let
            (state', effects') = toState <| action state
        in
            (state', Effects.batch [effects, effects'])

    update actions = List.foldl updateStep (initial, Effects.none) actions

    inputs = Signal.mergeMany <|
         Signal.map (\h -> List.foldl ((++) << .inputs) [] h) handlers
      :: mailbox.signal
      :: []
      -- :: List.map (Signal.map singleton) config.inputs

    -- update' actions (s,_) = List.foldl updateStep ([], Effects.none) actions
    -- result = Signal.foldp update' (["f"], Effects.none) inputs
    result = Signal.map update inputs
  in result

runRouter : Router state route -> Result state
runRouter router =
  let
    signalHandlers = Signal.map (pathHandlers router) path
    result = runHandlers router.config.init signalHandlers
    state = Signal.map fst result
    views = Signal.map (List.map .view) signalHandlers
    html  = Signal.map2 (\state viewList -> Maybe.withDefault (text "error") <| List.foldr (\view parsed -> view address state parsed) Nothing viewList) state views
  in
    {
      html  = html
    , state = state
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }

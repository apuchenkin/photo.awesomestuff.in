module Test.TestData where

import Html
import Dict exposing (Dict)
import Lib.Helpers exposing (noFx)
import Lib.Types    exposing (Constraint, RawURL)
import Lib.Types exposing (WithRouter, Action, Response (..), Router (..), Handler, RouteConfig, RouterConfig)
import MultiwayTree exposing (Tree (..), Forest)

type Route = Home | Page | Subpage | NotFound

routeTree : Forest Route
routeTree = [
    Tree NotFound [],
    Tree Home [
      Tree Page [
        Tree Subpage []
      ]
    ]
  ]

type alias State = WithRouter Route
  {
    str: String,
    sum: Int
  }

config : Route -> RouteConfig Route State
config route = case route of
    Home        -> {
      segment = "/"
    , constraints = Dict.empty
    , handler = always handlerA
    }
    NotFound    -> {
      segment = "/404"
    , constraints = Dict.empty
    , handler = always handlerA
    }
    Page        -> {
      segment = ":category[/:subcategory]"
    , constraints = Dict.empty
    , handler = always handlerA
    }
    Subpage     -> {
      segment = "/photo/:photo"
    , constraints = Dict.empty
    , handler = always handlerA
    }

routeMap : Route -> (RawURL, Dict String Constraint)
routeMap route = (.segment <| config route, .constraints <| config route)

init : State
init = {
    router = {
      route = Nothing
    , params = Dict.empty
    , cache = {unwrap = Dict.empty, rawUrl = Dict.empty, traverse = Dict.empty}
    },
    str = "",
    sum = 0
  }

routerConfig : RouterConfig Route State
routerConfig = {
    init      = init,
    useCache  = True,
    fallback  = (NotFound, Dict.empty),
    fallbackHtml  = Html.text "error",
    routes    = routeTree,
    config    = config,
    inits  = [],
    inputs = []
  }
------------ actions ----------------------
noAction : Action State
noAction state = Response <| noFx state

succ : Action State
succ state = Response <| noFx {state | sum = state.sum + 1}

append : String -> Action State
append string state = Response <| noFx {state | str = state.str ++ string}

------------ handlers ----------------------
handlerA : Handler State
handlerA = {
    view = \address state parsed -> Nothing,
    actions = [
      noAction
    ]
  }

handlerB : Handler State
handlerB = {
    view = \address state parsed -> Nothing,
    actions = [
      succ
    ]
  }

handlerC : Handler State
handlerC = {
    view = \address state parsed -> Nothing,
    actions = [
      succ,
      succ
    ]
  }

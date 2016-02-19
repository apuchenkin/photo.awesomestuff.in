module Test.TestData where

import Dict exposing (Dict)
import Lib.Helpers exposing (noFx)
import Lib.Types    exposing (Constraint, RawURL)
import Lib.Types exposing (WithRouter, Action, Response (..), Router (..), Handler, RouteConfig)
import MultiwayTree exposing (Tree (..), Forest)

type Route = Home | Page | Subpage | NotFound

routeMap : Route -> (RawURL, Dict String Constraint)
routeMap route = case route of
  Home        -> ("/", Dict.empty)
  NotFound    -> ("/404", Dict.empty)
  Page        -> (":category[/:subcategory]", Dict.empty)
  Subpage     -> ("/photo/:photo", Dict.empty)

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

-- config : Route -> RouteConfig Route State
-- config route = case route of
--     Home        -> ("/", Dict.empty)
--     NotFound    -> ("/404", Dict.empty)
--     Page        -> (":category[/:subcategory]", Dict.empty)
--     Subpage     -> ("/photo/:photo", Dict.empty)

noAction : Action State
noAction state = Response <| noFx state

succ : Action State
succ state = Response <| noFx {state | sum = state.sum + 1}

append : String -> Action State
append string state = Response <| noFx {state | str = state.str ++ string}

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

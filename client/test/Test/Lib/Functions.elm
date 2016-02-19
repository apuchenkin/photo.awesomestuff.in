module Test.Lib.Functions where

import ElmTest      exposing (..)
import Lib.Types exposing (WithRouter, Action, Response (..), Router (..), Handler)
import MultiwayTree exposing (Tree (..), Forest)
import Lib.Helpers exposing (noFx)
import Lib.Functions exposing (..)
import Dict

type Route = Locale | Home | NotFound | Static String | Category | Photo

routes : Forest Route
routes = [
    Tree Locale [
      Tree Home [
        Tree NotFound [],
        Tree (Static "about") [],
        Tree (Static "contacts") [],
        Tree Category [
          Tree Photo []
        ]
      ]
    ]
  ]

type alias State = WithRouter Route
  {
    str: String,
    sum: Int
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

actionA : Action State
actionA state = Response <| noFx state

actionB : Action State
actionB state = Response <| noFx state

handlerA : Handler State
handlerA = {
    view = \address state parsed -> Nothing,
    actions = [
      actionA
    ]
  }

handlerB : Handler State
handlerB = {
    view = \address state parsed -> Nothing,
    actions = [
      actionB
    ]
  }

handlerC : Handler State
handlerC = {
    view = \address state parsed -> Nothing,
    actions = [
      actionA,
      actionB
    ]
  }

testSuite : Test
testSuite = suite "Functions" [
    testRunHandlers
  ]

{-| Private -}
testRunHandlers : Test
testRunHandlers = suite "runHandlers"
  [
    test "lenth 2" <| flip assertEqual (List.length <| runHandlers [handlerA, handlerB]) 2
  , test "lenth 3" <| flip assertEqual (List.length <| runHandlers [handlerA, handlerB, handlerC]) 3
  ]
--
-- {-| Private -}
-- testChainAction : Test
-- testChainAction = suite "chainAction"
--   [
--     test "non-wrapped0" <| flip assertEqual (chainAction actionA actionB) (noFx init)
--   ]

{-| Private -}
testRunAction : Test
testRunAction = suite "runAction"
  [
    test "non-wrapped0" <| flip assertEqual (runAction actionA (noFx init)) (noFx init)
  ]

--
-- chainAction : Action state -> Action state -> Action state
-- chainAction action1 action2 state =
--
-- prepareCache : (WithRouter route state) -> RouterConfig route (WithRouter route state) -> (WithRouter route state)
-- prepareCache state config =
--
-- render : Router route (WithRouter route state) -> (WithRouter route state) -> Html
-- render router state =
--
-- setUrl : Router route (WithRouter route state) -> RouterState route -> String -> Action (WithRouter route state)
-- setUrl router state url =
--
-- setRoute : Router route (WithRouter route state) -> Route route -> Action (WithRouter route state)
-- setRoute router route state =
--
-- transition : Router route (WithRouter route state) -> Transition route (WithRouter route state)
-- transition router from to state =
--
-- getHandlers : Router route state -> RouterState route -> Maybe (Route route) -> Route route -> List (Handler state)
-- getHandlers router state from to =
--
-- matchRoute : RouterConfig route state -> RouterState route -> String -> Maybe (Route route)
-- matchRoute config state url =

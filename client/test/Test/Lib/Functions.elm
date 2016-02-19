module Test.Lib.Functions where

import ElmTest      exposing (..)
import Lib.Types exposing (WithRouter, Action, Response (..), Router (..), Handler, RouteConfig)
import Lib.Helpers exposing (noFx)
import Lib.Functions exposing (..)
import Test.TestData exposing (..)

testSuite : Test
testSuite = suite "Functions" [
    testRunAction,
    testCombineActions
  ]

testRunAction : Test
testRunAction = suite "runAction"
  [
    test "noAction"
      <| assertEqual init
      <| let (r,_) = runAction noAction (noFx init) in r
  , test "succ"
      <| assertEqual 1
      <| let (r,_) = runAction succ (noFx init) in r.sum
  , test "succ"
      <| assertEqual "foo"
      <| let (r,_) = runAction (append "foo") (noFx init) in r.str
  ]

testCombineActions : Test
testCombineActions = suite "combineActions"
  [
    test "noAction"
      <| assertEqual init
      <| let (Response (result,_)) = (combineActions [noAction, noAction, noAction]) init in result
  , test "one succ"
      <| assertEqual 1
      <| let (Response (result,_)) = (combineActions [succ, noAction]) init in result.sum
  , test "two succ"
      <| assertEqual 2
      <| let (Response (result,_)) = (combineActions [succ, succ]) init in result.sum
  , test "append order"
      <| assertEqual "ABC"
      <| let (Response (result,_)) = (combineActions [append "A", append "B", append "C"]) init in result.str
  , test "combined"
      <| assert
      <| let (Response (result,_)) = (combineActions [succ, append "A", succ, append "B"]) init
      in result.str == "AB" && result.sum == 2
  ]

-- prepareCache : RouterConfig route state -> RouterCache route
testPrepareCache : Test
testPrepareCache = suite "prepareCache"
  [
    test "pre" <| assertEqual (noFx init) <| (runAction noAction (noFx init))
  ]

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

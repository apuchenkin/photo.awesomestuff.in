module Test.Lib.Functions where

import Dict
import ElmTest      exposing (..)
import Lib.Types exposing (WithRouter, Action, Response (..), Router (..), Handler, RouteConfig)
import Lib.Helpers exposing (noFx)
import Lib.Functions exposing (..)
import Test.TestData exposing (..)

testSuite : Test
testSuite = suite "Functions"
  [ testRunAction
  , testCombineActions
  , testPrepareCache
  , testRender
  , testSetUrl
  , testSetRoute
  , testTransition
  , testGetHandlers
  , testMatchRoute
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

-- prepareCache : (route -> RawSegment) -> Forest route -> RouterCache route
testPrepareCache : Test
testPrepareCache =
  let cache = prepareCache (fst << routeMap) routeTree
  in suite "prepareCache"
  [
    test "not empty" <| assertNotEqual Dict.empty cache.rawUrl
  , test "not empty" <| assertNotEqual Dict.empty cache.unwrap
  , test "not empty" <| assertNotEqual Dict.empty cache.traverse
  ]

--
-- render : Router route (WithRouter route state) -> (WithRouter route state) -> Html
-- render router state =
testRender : Test
testRender = test "TODO: implement" pass

-- setUrl : Router route (WithRouter route state) -> RouterState route -> String -> Action (WithRouter route state)
-- setUrl router state url =
testSetUrl : Test
testSetUrl = test "TODO: implement" pass

-- setRoute : Router route (WithRouter route state) -> Route route -> Action (WithRouter route state)
-- setRoute router route state =
testSetRoute : Test
testSetRoute = test "TODO: implement" pass

-- transition : Router route (WithRouter route state) -> Transition route (WithRouter route state)
-- transition router from to state =
testTransition : Test
testTransition = test "TODO: implement" pass

-- getHandlers : Router route state -> RouterState route -> Maybe (Route route) -> Route route -> List (Handler state)
-- getHandlers router state from to =
testGetHandlers : Test
testGetHandlers = test "TODO: implement" pass

{-| see `Test.Lib.Matcher` - `testMatch` for test resultst -}
testMatchRoute : Test
testMatchRoute = test "covered" pass

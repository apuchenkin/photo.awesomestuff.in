module Test.Lib.Functions where

import Dict
import Html
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

-- render : Router route (WithRouter route state) -> Html -> (WithRouter route state) ->  Html
testRender : Test
testRender =
  let
   fallback = Html.text "error"
   state' route = let rs = init.router in {init | router = {rs | route = Just route}}
  in suite "render"
  [
    test "fail render"
      <| assertEqual fallback
      <| render router fallback init
  , test "render home"
      <| assertEqual (toString <| Html.text "handlerA")
      <| toString <| render router fallback (state' Home)
  , test "render Page"
      <| assertEqual (toString <| Html.text "0")
      <| toString <| render router fallback (state' Page)
  , test "render Subpage"
      <| assertEqual (toString <| Html.text "")
      <| toString <| render router fallback (state' Subpage)
  ]

-- setRoute : Router route (WithRouter route state) -> Route route -> Action (WithRouter route state)
testSetRoute : Test
testSetRoute =
  let
   state' route = let rs = init.router in {init | router = {rs | route = Just route}}
  in suite "setRoute"
  [
    test "route setted"
      <| assertEqual (Just Home)
      <| let (Response (result,_)) = setRoute router (Home, Dict.empty) init in result.router.route
  , test "route setted"
      <| assertEqual (Just Page)
      <| let (Response (result,_)) = setRoute router (Page, Dict.empty) (state' NotFound) in result.router.route
  , test "params setted"
      <| assertEqual (Dict.fromList [("param1", "value1")])
      <| let (Response (result,_)) = setRoute router (Subpage, Dict.fromList [("param1", "value1")]) init in result.router.params
  , test "route actions"
      <| assertEqual 1
      <| let (Response (result,_)) = setRoute router (Page, Dict.empty) init in result.sum
  , test "route actions"
      <| assertEqual (2,"foo")
      <| let (Response (result,_)) = setRoute router (Subpage, Dict.empty) init in (result.sum, result.str)
  , test "route actions"
      <| assertEqual (1,"foo")
      <| let (Response (result,_)) = setRoute router (Subpage, Dict.empty) (state' Page) in (result.sum, result.str)
  , test "route actions"
      <| assertEqual (0,"")
      <| let (Response (result,_)) = setRoute router (Subpage, Dict.empty) (state' Subpage) in (result.sum, result.str)
  ]

-- getHandlers : Router route state -> RouterCache route -> Maybe (Route route) -> Route route -> List (Handler state)
testGetHandlers : Test
testGetHandlers = suite "getHandlers"
  [
    test "length"
      <| assertEqual 1
      <| List.length <| getHandlers router init.router.cache Nothing (Home, Dict.empty)
  , test "length"
      <| assertEqual 3
      <| List.length <| getHandlers router init.router.cache Nothing (Subpage, Dict.empty)
  , test "no transition - no handlers"
      <| assertEqual 0
      <| List.length <| getHandlers router init.router.cache (Just (Home, Dict.empty)) (Home, Dict.empty)
  , test "unmatched params has no effects"
      <| assertEqual 0
      <| List.length <| getHandlers router init.router.cache (Just (Home, Dict.empty)) (Home, Dict.fromList [("param1", "value1")])
  , test "matched params does matter"
      <| assertEqual 1
      <| List.length <| getHandlers router init.router.cache (Just (Page, Dict.fromList [("category", "bar")])) (Page, Dict.fromList [("category", "foo")])
  , test "matched params does matter"
      <| assertEqual 2
      <| List.length <| getHandlers router init.router.cache (Just (Subpage, Dict.fromList [("category", "bar")])) (Subpage, Dict.fromList [("category", "foo")])
  ]

-- setUrl : Router route (WithRouter route state) -> RouterCache route -> String -> Action (WithRouter route state)
testSetUrl : Test
testSetUrl = test "setUrl: covered by matchRoute and setRoute" pass

{-| see `Test.Lib.Functions` - `testSetRoute` for test resultst -}
testTransition : Test
testTransition = test "covered" pass

{-| see `Test.Lib.Matcher` - `testMatch` for test resultst -}
testMatchRoute : Test
testMatchRoute = test "covered" pass

module Test.Lib.Matcher where

import Dict
import MultiwayTree as Tree exposing (Tree (..), Forest)
import ElmTest      exposing (..)
import Lib.Matcher  exposing (..)

type Route = Home | Category | Photo | NotFound

routeMap : Route -> RawURL
routeMap route = case route of
  Home      -> "/"
  NotFound  -> "/404"
  Category  -> ":category[/:subcategory]"
  Photo     -> "/photo/:photo"

routeTree : Forest Route
routeTree = [
    Tree NotFound [],
    Tree Home [
      Tree Category [
        Tree Photo []
      ]
    ]
  ]

testMatcher : Test
testMatcher = suite "matcher" [
    testUnwrap,
    testParseUrlParams,
    testMatch,
    testBuildUrl
  ]

{-| Private -}
testUnwrap : Test
testUnwrap = suite "unwrap"
  [
    test "non-wrapped0" <| flip assertEqual (unwrap "/url")                     ["/url"],
    test "non-wrapped1" <| flip assertEqual (unwrap "static/static2")           ["static/static2"],
    test "non-wrapped2" <| flip assertEqual (unwrap "static/:static2")          ["static/:static2"],
    test "non-wrapped3" <| flip assertEqual (unwrap ":static/static2")          [":static/static2"],

    test "wrapped1"     <| flip assertEqual (unwrap "[test]")                   ["test", ""],
    test "wrapped2"     <| flip assertEqual (unwrap "[/:test]")                 ["/:test", ""],
    test "wrapped3"     <| flip assertEqual (unwrap "/path[/test]")             ["/path/test", "/path"],
    test "wrapped4"     <| flip assertEqual (unwrap "[/:substring]/:string")    ["/:substring/:string", "/:string"],

    test "two-wrapped"  <| flip assertEqual (unwrap "[/:string[/:substring]]")          ["/:string/:substring", "/:string", ""],
    test "two-wrapped"  <| flip assertEqual (unwrap "/path[/:string[/:substring]]")     ["/path/:string/:substring", "/path/:string", "/path"],
    test "two-wrapped"  <| flip assertEqual (unwrap "/path[/:string[/:substring]]/sub") ["/path/:string/:substring/sub", "/path/:string/sub", "/path/sub"],
    test "two-wrapped"  <| flip assertEqual (unwrap "/path[/:string][/:substring]")     ["/path/:string/:substring", "/path/:substring", "/path/:string", "/path"]
  ]

{-| Private -}
testParseUrlParams : Test
testParseUrlParams = suite "parseUrlParams"
  [
    test "plain"      <| flip assertEqual (parseUrlParams "/url" "/url")                          (Ok Dict.empty, ""),
    test "param"      <| flip assertEqual (parseUrlParams "/:param" "/value")                     (Ok (Dict.fromList [("param","value")]), ""),
    test "combined1"  <| flip assertEqual (parseUrlParams "/path/:param" "/path/value")           (Ok (Dict.fromList [("param","value")]), ""),
    test "combined2"  <| flip assertEqual (parseUrlParams "/:path/param" "/value/param")          (Ok (Dict.fromList [("path","value")]), ""),
    test "combined3"  <| flip assertEqual (parseUrlParams "/:path/:param" "/value1/value2")       (Ok (Dict.fromList [("path","value1"), ("param","value2")]), ""),
    test "fail"       <| flip assertEqual (parseUrlParams "/url" "/path")                         (Err (["expected \"/url\""]),"/path")
  ]

testMatch : Test
testMatch = suite "match"
  [
    test "match" <| flip assertEqual (match routeMap routeTree "/")                     <| Just (Home, Dict.empty),
    test "match" <| flip assertEqual (match routeMap routeTree "/param")                <| Just (Category, (Dict.fromList [("category","param")])),
    test "match" <| flip assertEqual (match routeMap routeTree "/404")                  <| Just (NotFound, Dict.empty),
    test "match" <| flip assertEqual (match routeMap routeTree "/param/param2")         <| Just (Category, (Dict.fromList [("category","param"),("subcategory","param2")])),
    test "match" <| flip assertEqual (match routeMap routeTree "/param/photo/3")        <| Just (Photo,Dict.fromList [("category","param"),("photo","3")]),
    test "match" <| flip assertEqual (match routeMap routeTree "/param/param2/photo/4") <| Just (Photo,Dict.fromList [("category","param"),("subcategory","param2"),("photo","4")]),

    test "no-match" <| flip assertEqual (match routeMap routeTree "/param/param2/param3")     <| Nothing,
    test "no-match" <| flip assertEqual (match routeMap routeTree "/param/param2/photo/4/4")  <| Nothing
  ]

testBuildUrl : Test
testBuildUrl = suite "buildUrl" []

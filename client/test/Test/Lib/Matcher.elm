module Test.Lib.Matcher where

import Dict
import ElmTest      exposing (..)
import Lib.Matcher  exposing (..)


testMatcher : Test
testMatcher = suite "matcher" [
    testUnwrap,
    testParseUrlParams
  ]

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

testParseUrlParams : Test
testParseUrlParams = suite "parseUrlParams"
  [
    test "plain" <| flip assertEqual (parseUrlParams "/url" "/url")                         (Ok Dict.empty, ""),
    test "param" <| flip assertEqual (parseUrlParams "/:param" "/value")                    (Ok (Dict.fromList [("param","value")]), ""),
    test "combined1" <| flip assertEqual (parseUrlParams "/path/:param" "/path/value")      (Ok (Dict.fromList [("param","value")]), ""),
    test "combined2" <| flip assertEqual (parseUrlParams "/:path/param" "/value/param")     (Ok (Dict.fromList [("path","value")]), ""),
    test "combined3" <| flip assertEqual (parseUrlParams "/:path/:param" "/value1/value2")  (Ok (Dict.fromList [("path","value1"), ("param","value2")]), ""),
    test "fail" <| flip assertEqual (parseUrlParams "/url" "/path")                         (Err (["expected \"/url\""]),"/path")
  ]

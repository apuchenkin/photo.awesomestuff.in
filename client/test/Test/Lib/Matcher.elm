module Test.Lib.Matcher where

import ElmTest      exposing (..)
import Lib.Matcher  exposing (..)


testMatcher : Test
testMatcher = suite "matcher" [
        testUnwrap
    ]

testUnwrap : Test
testUnwrap =
    suite "unwrap"
        [
            test "non-wrapped0" <| assertEqual (unwrap "url")             ["url"],
            test "non-wrapped1" <| assertEqual (unwrap "static/static2")  ["static/static2"],
            test "non-wrapped2" <| assertEqual (unwrap "static/:static2") ["static/:static2"],
            test "non-wrapped3" <| assertEqual (unwrap ":static/static2") [":static/static2"],

            test "wrapped1"     <| assertEqual (unwrap "[test]")          ["test", ""],
            test "wrapped2"     <| assertEqual (unwrap "[:test]")         [":test", ""],
            test "wrapped3"     <| assertEqual (unwrap "/path[/test]")    ["/path/test", "/path"],
            test "wrapped3"     <| assertEqual (unwrap "/path[/:test]")   ["/path/:test", "/path"],

            test "two-wrapped"  <| assertEqual (unwrap "/path[/:string[/:substring]]") ["/path/:string/:substring", "/path/:string", "/path"],
            test "two-wrapped"  <| assertEqual (unwrap "/path[/:string][/:substring]") ["/path/:string/:substring", "/path/:string", "/path/:substring", "/path"]
        ]

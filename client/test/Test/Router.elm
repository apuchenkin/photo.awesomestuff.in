module Test.Router where

import Test.Lib.Matcher
import Test.Lib.Helpers
import Test.Lib.Functions

import ElmTest      exposing (Test, suite)

testSuite : Test
testSuite = suite "Router" [
    Test.Lib.Matcher.testSuite,
    Test.Lib.Functions.testSuite
  ]

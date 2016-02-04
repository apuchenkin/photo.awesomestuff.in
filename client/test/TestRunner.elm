module Main where

import Signal exposing (Signal)

import ElmTest exposing (consoleRunner)
import Console exposing (IO, run)
import Task

import Test.Lib.Matcher

console : IO ()
console = consoleRunner Test.Lib.Matcher.testMatcher

port runner : Signal (Task.Task x ())
port runner = run console

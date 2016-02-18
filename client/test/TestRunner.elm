module Main where

import Signal exposing (Signal)

import ElmTest exposing (consoleRunner)
import Console exposing (IO, run)
import Task

import Test.Router

console : IO ()
console = consoleRunner Test.Router.testSuite

port runner : Signal (Task.Task x ())
port runner = run console

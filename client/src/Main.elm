import Html exposing (div, text, Html, button)
import Task exposing (Task)
import Effects exposing (Never)
import Lib.Router exposing (..)
import History

result : SignalResult (List String)
result = runRouter History.path

main : Signal Html
main = result.html

port tasks : Signal (Task Never ())
port tasks = result.tasks -- Signal.map (\_ -> ) main

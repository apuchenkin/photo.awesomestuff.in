port module App.Ports exposing (..)

import Process
import Time exposing (Time)
import Task exposing (Task)
import App.Model exposing (State)
import Router.Helpers exposing (doNothing, noFx, performTask)
import Router.Types exposing (Action, Response (..))

port meta : App.Model.Meta -> Cmd msg

port photos : Bool -> Cmd msg

port transition :  String  -> Cmd msg

portCmd : Cmd msg -> Cmd (Action State)
portCmd cmd = Cmd.map (always doNothing) cmd

toTask : Cmd (Action State) -> Action State
toTask cmd state = Response (state, cmd)

-- A dummy command is executed after a tick
portDelayCmd : Time -> Cmd msg -> Cmd (Action State)
portDelayCmd time cmd = performTask <| Process.sleep time `Task.andThen` (always <| Task.succeed <| toTask <| portCmd cmd)

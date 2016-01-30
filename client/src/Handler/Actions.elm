module Handler.Actions where

import Http
import Json.Decode
import Effects exposing (Never)
import History exposing (setPath)
import Task exposing (Task)
import Lib.Router exposing (..)
type alias State = List String

forward : Action State
forward state =
  let
    tsk  = setPath "/404"
    tsk' = Task.andThen (Task.toMaybe tsk) (\r -> Task.succeed (\a -> DirtyState (a, Effects.none)))
  in DirtyState (state, Effects.task <| tsk')


loadCategories : Action State
loadCategories state =
  let
    tsk = Http.get Json.Decode.string ("http://photo.awesomestuff.in/api/v1/category")
    tsk' = Task.andThen (Task.toMaybe tsk) (\r -> Task.succeed (updateCategories <| Maybe.withDefault "supgf" r))
  in DirtyState (state, Effects.task <| tsk')

updateCategories : String -> Action State
updateCategories s state = DirtyState ([s], Effects.none)

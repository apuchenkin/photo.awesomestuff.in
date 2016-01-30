module Handler.Actions where

import Http
import Json.Decode  as Json exposing ((:=))
import Effects exposing (Never)
import History exposing (setPath)
import Task exposing (Task)
import Lib.Router exposing (..)

type alias State = List Category

type alias Category = {
    id: Int,
    name: String,
    title: String
  }

decodeCategories : Json.Decoder (List Category)
decodeCategories = Json.list <| Json.object3 Category
  ("id"     := Json.int)
  ("name"   := Json.string)
  ("title"  := Json.string)

forward : Action State
forward state =
  let
    tsk  = setPath "/404"
    tsk' = Task.andThen (Task.toMaybe tsk) (\r -> Task.succeed (\a -> DirtyState (a, Effects.none)))
  in DirtyState (state, Effects.task <| tsk')


loadCategories : Action State
loadCategories state =
  let
    tsk = Http.get decodeCategories ("/api/v1/category")
    tsk' = Task.andThen (Task.toMaybe tsk) (\r -> Task.succeed (updateCategories <| Maybe.withDefault [] r))
  in DirtyState (state, Effects.task <| tsk')

updateCategories : List Category -> Action State
updateCategories categories state = DirtyState (categories, Effects.none)

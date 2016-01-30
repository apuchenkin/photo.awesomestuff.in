module Handler.Actions where

import Http
import Json.Decode  as Json exposing ((:=))
import Effects exposing (Never)
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

loadCategories : Action State
loadCategories state =
  let
    tsk = Http.get decodeCategories ("/api/v1/category")
    tsk' = Task.andThen (Task.toMaybe tsk) (\r -> Task.succeed (updateCategories <| Maybe.withDefault [] r))
  in DirtyState (state, Effects.task <| tsk')

updateCategories : List Category -> Action State
updateCategories categories state = DirtyState (categories, Effects.none)

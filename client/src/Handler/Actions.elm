module Handler.Actions where

import Http
import Json.Decode  as Json exposing ((:=))
import Effects exposing (Never)
import Task exposing (Task)
import Lib.Router exposing (..)
import Handler.Routes exposing (Route)

type alias State = WithRouter Route
  {categories: List Category}


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
  in Response (state, Effects.task <| tsk')

updateCategories : List Category -> Action State
updateCategories categories state = Response ({state | categories = categories}, Effects.none)

module Handler.Actions where

import Http
import Dict exposing (Dict)
import Json.Decode  as Json exposing ((:=))
import Effects exposing (Never)
import Task exposing (Task)
import Lib.Types exposing (WithRouter, Action, Response (..))
import Handler.Routes exposing (Route)
import Lib.Helpers exposing (noFx)

type alias State = WithRouter Route
  {
    categories: Dict String Category,
    isLoading: Bool
  }

type ParentCategory = Left Int | Right Category
type alias Category = {
    id: Int,
    name: String,
    title: String,
    parent: Maybe ParentCategory
  }

decodeCategories : Json.Decoder (List Category)
decodeCategories = Json.list <| Json.object4 Category
  ("id"     := Json.int)
  ("name"   := Json.string)
  ("title"  := Json.string)
  (Json.maybe ("parent" := Json.map Left Json.int))

loadCategories : Action State
loadCategories state =
  let
    _ = Debug.log "loadCategories" state
    tsk = Http.get decodeCategories ("/api/v1/category")
    tsk' = Task.andThen (Task.toMaybe tsk) (\categories -> Task.succeed (updateCategories <| Maybe.withDefault [] categories))
  in Response ({state | isLoading = True}, Effects.task tsk')

updateCategories : List Category -> Action State
updateCategories categories state =
  let
    _ = Debug.log "updateCategories" categories
    dict = Dict.fromList  <| List.map (\(c) -> (c.name, c)) categories
    dict' = Dict.fromList <| List.map (\(c) -> (c.id, c))   categories

    castegoris' = Dict.map (\k v -> {v | parent = Maybe.map (\p ->
      case p of
        Right _ -> p
        Left  pidx -> case (Dict.get pidx dict') of
          Nothing -> p
          Just p' -> Right p'
      ) v.parent}) dict
  in
    Response <| noFx {state | isLoading = False, categories = castegoris'}

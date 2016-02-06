module Handler.Actions where

import Http
import Either exposing (Either (..))
import Dict exposing (Dict)
import Json.Decode  as Json exposing ((:=))
import Effects exposing (Never)
import Task exposing (Task)
import Lib.Types exposing (WithRouter, Action, Response (..))
import Handler.Routes exposing (Route)
import Lib.Helpers exposing (noFx)

type alias Promise value = Either (Task Http.Error value) value

type alias State = WithRouter Route
  {
    categories: Dict String Category,
    isLoading: Bool
  }

-- type ParentCategory =
type Category = Category {
    id: Int,
    name: String,
    title: String,
    parent: Maybe (Either Int Category)
  }

type alias Photo = {
  id: Int,
  src: String,
  width: Int,
  height: Int
}

-- Category constuctor
category : Int -> String -> String -> Maybe (Either Int Category) -> Category
category id name title parent = Category {id = id, name = name, title = title, parent = parent}

decodeCategories : Json.Decoder (List Category)
decodeCategories = Json.list <| Json.object4 category
  ("id"     := Json.int)
  ("name"   := Json.string)
  ("title"  := Json.string)
  (Json.maybe ("parent" := Json.map Left Json.int))

decodePhotos : Json.Decoder (List Photo)
decodePhotos = Json.list <| Json.object4 Photo
  ("id"     := Json.int)
  ("src"    := Json.string)
  ("width"  := Json.int)
  ("height" := Json.int)

loadCategories : Action State
loadCategories state =
  let
    _ = Debug.log "loadCategories" state
    tsk = Http.get decodeCategories ("/api/v1/category")
    tsk' = Task.andThen (Task.toMaybe tsk) (\categories -> Task.succeed (updateCategories <| Maybe.withDefault [] categories))
  in Response ({state | isLoading = True}, Effects.task tsk')

loadPhotos : Action State
loadPhotos state =
  let
    _ = Debug.log "loadPhotos" state
    tsk = Http.get decodeCategories ("/api/v1/category/photo")
    tsk' = Task.andThen (Task.toMaybe tsk) (\categories -> Task.succeed (updateCategories <| Maybe.withDefault [] categories))
  in Response ({state | isLoading = True}, Effects.task tsk')

updateCategories : List Category -> Action State
updateCategories categories state =
  let
    _ = Debug.log "updateCategories" categories
    dict = Dict.fromList  <| List.map (\(Category c) -> (c.name, c)) categories
    dict' = Dict.fromList <| List.map (\(Category c) -> (c.id, c))   categories

    castegoris' = Dict.map (\name category -> Category {category | parent = Maybe.map (\p ->
      case p of
        Right _ -> p
        Left  pidx -> case (Dict.get pidx dict') of
          Nothing -> p
          Just p' -> Right <| Category p'
      ) category.parent}) dict
  in
    Response <| noFx {state | isLoading = False, categories = castegoris'}

module Handler.Actions where

import Http
import Date exposing (Date)
import Either exposing (Either (..))
import Dict exposing (Dict)
import Json.Decode  as Json exposing ((:=))
import Effects exposing (Never)
import Task exposing (Task)
import Lib.Types exposing (WithRouter, Action, Response (..), Router (..))
import Lib.Helpers exposing (noFx, chainAction)

import Handler.Config exposing (config)
import Handler.Routes as Routes exposing (Route)
import Handler.Locale as Locale exposing (Locale)

(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) = Maybe.andThen

type alias Promise value = Either (Task Http.Error value) value

type alias Meta = {
    title: String,
    links: List (String, String)
  }

type alias State = WithRouter Route
  {
    meta: Meta,
    locale: Locale,
    categories: Dict String Category,
    photos: List Photo,
    photo: Maybe Photo,
    isLoading: Bool,
    mouse: (Int, Int)
  }

-- type ParentCategory =
type Category = Category {
    id: Int,
    name: String,
    title: String,
    image: Maybe String,
    date: Maybe Date,
    parent: Maybe (Either Int Category)
  }

type alias Photo = {
  id: Int,
  src: String,
  width: Int,
  height: Int
}

createLinks : Router Route State -> Action State
createLinks router state =
  let
    (Router r) = router
    meta = state.meta
    default = flip Maybe.map state.router.route <| \ route ->
      ("x-default", config.hostname ++ r.buildUrl (route, Dict.remove "locale" state.router.params))
      :: (flip List.map Locale.locales
      <| \locale -> (Locale.toString locale, config.hostname ++ r.buildUrl (route, Dict.insert "locale" (Locale.toString locale) state.router.params))
      )

  in Response <| noFx {state | meta = {meta | links = Maybe.withDefault [] default}}

-- Category constuctor
category : Int -> String -> String -> Maybe String -> Maybe String -> Maybe (Either Int Category) -> Category
category id name title image date parent = Category {
    id = id,
    name = name,
    title = title,
    image = image,
    date  = date &> \d -> Result.toMaybe (Date.fromString d),
    parent = parent
  }

getCategory : State -> Maybe Category
getCategory state =
  let
    param = case Dict.get "subcategory" state.router.params of
      Nothing -> Dict.get "category" state.router.params
      c -> c
  in param &> flip Dict.get state.categories

decodeCategories : Json.Decoder (List Category)
decodeCategories = Json.list <| Json.object6 category
  ("id"     := Json.int)
  ("name"   := Json.string)
  ("title"  := Json.string)
  (Json.maybe ("image"  := Json.string))
  (Json.maybe ("date"  := Json.string))
  (Json.maybe ("parent" := Json.map Left Json.int))

decodePhoto : Json.Decoder Photo
decodePhoto = Json.object4 Photo
  ("id"     := Json.int)
  ("src"    := Json.string)
  ("width"  := Json.int)
  ("height" := Json.int)

decodePhotos : Json.Decoder (List Photo)
decodePhotos = Json.list decodePhoto

getRequest: Json.Decoder value -> String -> State -> Task Http.Error value
getRequest decoder url state = Http.fromJson decoder (Http.send Http.defaultSettings
  { verb = "GET"
  , headers = [("Accept-languadge", Locale.toString state.locale)]
  , url = url
  , body = Http.empty
  })

loadCategories : Router Route State -> Action State
loadCategories router state =
  let
    -- _ = Debug.log "loadCategories" state
    fetch = Task.toMaybe <| getRequest decodeCategories "/api/v1/category" state
    task = fetch `Task.andThen` \mcategories ->
      let
        categories = Maybe.withDefault [] mcategories
        update = updateCategories categories
        categoryParam =  case Dict.get "subcategory" state.router.params of
            Nothing -> Dict.get "category" state.router.params
            c -> c
        -- _ = Debug.log "categoryParam" state.router
        action = Maybe.withDefault update <| flip Maybe.map categoryParam <| \category ->
          update `chainAction` (loadPhotos router)
      in Task.succeed <| action --case state.router.route of
        -- Just (Routes.Category) ->
        -- Just (Routes.Photo)    -> update `chainAction` loadPhotos
        -- _ -> update

  in Response ({state | isLoading = True}, Effects.task task)

loadPhotos : Router Route State -> Action State
loadPhotos (Router router) state =
  let
    -- _ = Debug.log "loadPhotos" state
    task = case Dict.isEmpty state.categories of
      True   -> Nothing
      False  ->
        let
          category = getCategory state
          fetch = flip Maybe.map category <| \(Category c) ->
            let fetch = Task.toMaybe <| getRequest decodePhotos ("/api/v1/category/" ++ toString c.id ++ "/photo") state
            in fetch `Task.andThen` \photos -> Task.succeed <| updatePhotos <| Maybe.withDefault [] photos

        in Just <| Maybe.withDefault (Task.succeed <| router.forward (Routes.NotFound, Dict.empty)) fetch

  in Response ({state | isLoading = True}, Maybe.withDefault Effects.none <| Maybe.map Effects.task task)

loadPhoto : Action State
loadPhoto state =
  let
    photoId = Dict.get "photo" state.router.params
    task = flip Maybe.map photoId <| \pid ->
      let fetch = Task.toMaybe <| getRequest decodePhoto ("/api/v1/photo/" ++ pid) state
      in fetch `Task.andThen` \photo -> Task.succeed <| updatePhoto photo

  in Response ({state | isLoading = True}, Maybe.withDefault Effects.none <| Maybe.map Effects.task task)

updatePhotos : List Photo -> Action State
updatePhotos photos state = Response <| noFx {state | isLoading = False, photos = photos}

updatePhoto : Maybe Photo -> Action State
updatePhoto photo state = Response <| noFx {state | isLoading = False, photo = photo}

updateCategories : List Category -> Action State
updateCategories categories state =
  let
    -- _ = Debug.log "updateCategories" state
    dict  = Dict.fromList   <| List.map (\(Category c) -> (c.name, c)) categories
    dict' = Dict.fromList   <| List.map (\(Category c) -> (c.id, c))   categories

    castegoris' = Dict.map (\name category -> Category {category | parent = Maybe.map (\p ->
      case p of
        Right _ -> p
        Left  pidx -> case (Dict.get pidx dict') of
          Nothing -> p
          Just p' -> Right <| Category p'
      ) category.parent}) dict
    -- _ = Debug.log "cs" categories
  in
    Response <| noFx {state | isLoading = False, categories = castegoris'}

setLocale : Router Route State -> Action State
setLocale (Router router) state =
  let
    _ = Debug.log "locale" locale
    locale = Dict.get "locale" state.router.params
    route = Maybe.withDefault Routes.Home state.router.route
    act = case locale of
      Nothing -> router.forward (route, Dict.fromList [("locale", Locale.toString state.locale)])
      Just l  -> \state -> Response <| noFx {state | locale = Locale.fromString l}

  in act state

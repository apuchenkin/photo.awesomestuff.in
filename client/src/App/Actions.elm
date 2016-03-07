module App.Actions where

import Http
import Date exposing (Date)
import Time exposing (Time)
import Either exposing (Either (..))
import Dict exposing (Dict)
import Json.Decode  as Json exposing ((:=))
import Effects exposing (Never)
import Task exposing (Task)
import Router.Types exposing (WithRouter, Action, Response (..), Router)
import Router.Helpers exposing (noFx, chainAction, doNothing)

import App.Config exposing (config)
import App.Routes as Routes exposing (Route)
import App.Locale as Locale exposing (Locale)

(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) = Maybe.andThen

type alias Promise value = Either (Task Http.Error value) value

type alias Meta = {
    title: String,
    links: List (String, String)
  }

type alias State = WithRouter Route {
    meta: Meta
  , locale: Locale
  , categories: Dict String Category
  , photos: Dict Int Photo
  , photo: Maybe Photo
  , isLoading: Bool
  , time: Time
  , window: (Int, Int)
  }

-- type ParentCategory =
type Category = Category {
    id: Int
  , name: String
  , title: String
  , image: Maybe String
  , date: Maybe Date
  , parent: Maybe (Either Int Category)
  }

type alias Photo = {
    id: Int
  , src: String
  , width: Int
  , height: Int
  , views: Int
  , caption: Maybe String
  , author: Maybe Author
  }

type alias Author = {
    name: String
  }

transition : Router Route State -> from -> to -> Action State
transition router _ _ = createLinks router

startLoading : Action State
startLoading state =
  let _ = Debug.log "startLoading" ()
  in Response <| noFx { state | isLoading = True}

stopLoading : Action State
stopLoading state =
  let _ = Debug.log "stopLoading" ()
  in Response <| noFx { state | isLoading = False}


setTime : Time -> Action State
setTime time state =
  let _ = Debug.log "setTime" time
  in Response <| noFx {state | time = time}

setDims : (Int, Int) -> Action State
setDims dims state =
  let _ = Debug.log "setDims" dims
  in Response <| noFx {state | window = dims}

setLocale : Locale -> Action State
setLocale locale state =
  let _ = Debug.log "setLocale" locale
  in Response <| noFx {state | locale = locale}

createLinks : Router Route State -> Action State
createLinks router state =
  let
    _ = Debug.log "createLinks" ()
    meta = state.meta
    links = flip Maybe.map state.router.route <| \ route ->
      ("x-default", config.hostname ++ router.buildUrl (route, Dict.remove "locale" state.router.params))
      :: (flip List.map Locale.locales
      <| \locale -> (Locale.toString locale, config.hostname ++ router.buildUrl (route, Dict.insert "locale" (Locale.toString locale) state.router.params))
      )

  in Response <| noFx {state | meta = {meta | links = Maybe.withDefault [] links}}

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
decodePhoto = Json.object7 Photo
  ("id"     := Json.int)
  ("src"    := Json.string)
  ("width"  := Json.int)
  ("height" := Json.int)
  ("views"  := Json.int)
  (Json.maybe ("caption" := Json.string))
  (Json.maybe ("author"  := Json.object1 Author ("name" := Json.string)))

decodePhotos : Json.Decoder (List Photo)
decodePhotos = Json.list decodePhoto

getRequest: Json.Decoder value -> String -> State -> Task Http.Error value
getRequest decoder url state = Http.fromJson decoder (Http.send Http.defaultSettings
  { verb = "GET"
  , headers = [("Accept-language", Locale.toString state.locale)]
  , url = url
  , body = Http.empty
  })

loadCategories : Router Route State -> Action State
loadCategories router state =
  let
    _ = Debug.log "loadCategories" ()
    fetch = Task.toMaybe <| getRequest decodeCategories (config.apiEndpoint ++ "/category") state
    task = fetch `Task.andThen` \mcategories ->
      let
        categories = Maybe.withDefault [] mcategories
        update = stopLoading `chainAction` updateCategories categories
        categoryParam =  case Dict.get "subcategory" state.router.params of
            Nothing -> Dict.get "category" state.router.params
            c -> c
        action = Maybe.withDefault update <| flip Maybe.map categoryParam <| \category ->
          update
            `chainAction` (checkCategory router category)
            `chainAction` (loadPhotos router)

      in Task.succeed <| action

  in Response ({state | isLoading = True, categories = Dict.empty}, Effects.task task)

checkCategory : Router Route State -> String -> Action State
checkCategory router category state =
    case Dict.get category state.categories of
      Nothing -> router.redirect (Routes.Home, Dict.empty) state
      _ -> doNothing state

loadPhotos : Router Route State -> Action State
loadPhotos router state =
  let
    _ = Debug.log "loadPhotos" ()
    task = case Dict.isEmpty state.categories of
      True   -> Nothing
      False  ->
        let
          category = getCategory state
          default = Task.succeed <| stopLoading `chainAction` router.redirect (Routes.NotFound, Dict.empty)
          updateTask photos = Task.succeed <| stopLoading `chainAction` (updatePhotos <| Maybe.withDefault [] photos)
          fetchTask (Category c) = Task.toMaybe <| getRequest decodePhotos (config.apiEndpoint ++ "/category/" ++ toString c.id ++ "/photo") state
          task = flip Maybe.map category
            <| \c -> fetchTask c `Task.andThen` updateTask

        in Just <| Maybe.withDefault default task

  in Response ({ state | isLoading = Maybe.withDefault False <| Maybe.map (always True) task}, Maybe.withDefault Effects.none <| Maybe.map Effects.task task)

loadPhoto : Action State
loadPhoto state =
  let
    _ = Debug.log "loadPhoto" ()
    photoId = Dict.get "photo" state.router.params
    task = flip Maybe.map photoId <| \pid ->
      let fetch = Task.toMaybe <| getRequest decodePhoto (config.apiEndpoint ++ "/photo/" ++ pid) state
      in fetch `Task.andThen` \photo -> Task.succeed <| updatePhoto photo

  in Response ({state | isLoading = True, photo = Nothing}, Maybe.withDefault Effects.none <| Maybe.map Effects.task task)

updatePhotos : List Photo -> Action State
updatePhotos photos state =
  let
    _ = Debug.log "updatePhotos" ()
    dict = Dict.fromList <| List.map (\p -> (p.id, p)) photos
  in Response <| noFx {state | photos = dict}

updatePhoto : Maybe Photo -> Action State
updatePhoto photo state = Response <| noFx {state | photo = photo}

updateCategories : List Category -> Action State
updateCategories categories state =
  let
    _ = Debug.log "updateCategories" ()
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
    Response <| noFx {state | categories = castegoris'}

resolveLocale : Router Route State -> Action State
resolveLocale router state =
  let
    locale = Dict.get "locale" state.router.params
    _ = Debug.log "resolveLocale" locale
    route = Maybe.withDefault Routes.Home state.router.route
    params = Dict.fromList [("locale", Locale.toString state.locale)]
  in case locale of
    Nothing -> router.redirect (route, Dict.union params state.router.params) state
    Just l  -> Response <| noFx {state | locale = Locale.fromString l}

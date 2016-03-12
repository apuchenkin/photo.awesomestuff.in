module App.Actions where

import Http
import Random
import Time exposing (Time)
import Either exposing (Either (..))
import Dict exposing (Dict)
import Json.Decode  as Json exposing ((:=))
import Effects exposing (Never)
import Task exposing (Task)
import Router.Types exposing (WithRouter, Action, Response (..), Router)
import Router.Helpers exposing (noFx, chainAction, doNothing)

import App.Model exposing (..)
import App.Config exposing (config)
import App.Routes as Routes exposing (Route)
import App.Locale as Locale exposing (Locale)
import Service.Photo exposing (refinePhotos)

mapDefault : Maybe a -> b -> (a -> b) -> b
mapDefault maybe default map = Maybe.withDefault default <| Maybe.map map maybe

succ : number -> number
succ a = a + 1

pred : number -> number
pred a = a - 1

transition : Router Route State -> from -> to -> Action State
transition router _ _ = resetMeta `chainAction` resetLoading `chainAction` createLinks router

isLoading : State -> Bool
isLoading state = state.isLoading /= 0

resetLoading : Action State
resetLoading state =
  let _ = Debug.log "resetLoading" ()
  in Response <| noFx { state | isLoading = 0}

startLoading : Action State
startLoading state =
  let _ = Debug.log "startLoading" ()
  in Response <| noFx { state | isLoading = succ state.isLoading}

setTitle : Maybe String -> Action State
setTitle title state =
  let
    _ = Debug.log "setTitle" title
    meta = state.meta
    title' = Maybe.withDefault config.title <| Maybe.map (\t -> Locale.i18n state.locale "TITLE" [t]) title
  in Response <| noFx {state | meta = {meta | title = title'}}

setDescription : Maybe String -> Action State
setDescription desc state =
  let
    _ = Debug.log "setDescription" desc
    meta = state.meta
    desc' = Maybe.withDefault (Locale.i18n state.locale "META.DESCRIPTION" []) desc
  in Response <| noFx {state | meta = {meta | description = desc'}}

resetMeta : Action State
resetMeta = setTitle Nothing `chainAction` setDescription Nothing

setMetaFromCategory : Category -> Action State
setMetaFromCategory (Category c) =
    setTitle (Just c.title)
    `chainAction`
    setDescription (Maybe.withDefault c.description <| Maybe.map Just c.shortDescription)

setMetaFromPhoto : Photo -> Action State
setMetaFromPhoto photo =
    setTitle photo.caption
    `chainAction`
    (\state -> setDescription (Maybe.map2 (\author caption -> Locale.i18n state.locale "META.DESCRIPTION.PHOTO" [author.name, caption]) photo.author photo.caption) state)

stopLoading : Action State
stopLoading state =
  let _ = Debug.log "stopLoading" ()
  in Response <| noFx { state | isLoading = if state.isLoading > 0 then pred state.isLoading else 0}

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

getCategory : State -> Maybe Category
getCategory state =
  let
    param = case Dict.get "subcategory" state.router.params of
      Nothing -> Dict.get "category" state.router.params
      c -> c
  in param &> flip Dict.get state.categories

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
    task = Task.map (\f -> startLoading `chainAction` f) <| fetch `Task.andThen` \mcategories ->
      let
        categories = Maybe.withDefault [] mcategories
        update = updateCategories categories `chainAction` stopLoading
        categoryParam =  case Dict.get "subcategory" state.router.params of
            Nothing -> Dict.get "category" state.router.params
            c -> c
        action = Maybe.withDefault update <| flip Maybe.map categoryParam <| \category ->
          update
            `chainAction` (checkCategory router category)
            `chainAction` (loadPhotos router)
            `chainAction` (\state -> mapDefault (getCategory state) (doNothing state) (\c -> setMetaFromCategory c state))

      in Task.succeed <| action

  in Response ({state | categories = Dict.empty}, Effects.task task)

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
            <| \c ->  fetchTask c `Task.andThen` updateTask

        in Just <| Maybe.withDefault default task
    (Response (state', _)) = startLoading state

  in Response (mapDefault task state (always state'), mapDefault task Effects.none Effects.task)

loadPhoto : Action State
loadPhoto state =
  let
    _ = Debug.log "loadPhoto" ()
    photoId = Dict.get "photo" state.router.params
    task = flip Maybe.map photoId <| \pid ->
      let fetch = Task.toMaybe <| getRequest decodePhoto (config.apiEndpoint ++ "/photo/" ++ pid) state
      in fetch `Task.andThen` \photo -> Task.succeed <| updatePhoto photo
    (Response (state', _)) = resetLoading `chainAction` startLoading <| state
    state'' = mapDefault task state (always state')

  in Response ({state'' | photo = Nothing}, mapDefault task Effects.none Effects.task)

updatePhotos : List Photo -> Action State
updatePhotos photos state =
  let
    _ = Debug.log "updatePhotos" ()
    seed = Random.initialSeed <| floor <| Time.inSeconds state.time
    photos' = refinePhotos seed photos
    -- dict = Dict.fromList <| List.map (\p -> (p.id, p)) photos'
  in Response <| noFx {state | photos = photos'}

updatePhoto : Maybe Photo -> Action State
updatePhoto photo = (\state -> Response <| noFx {state | photo = photo})
  `chainAction` (\state -> mapDefault photo (doNothing state) (\p -> setMetaFromPhoto p state))

updateCategories : List Category -> Action State
updateCategories categories state =
  let
    _ = Debug.log "updateCategories" ()
    dict  = Dict.fromList   <| List.map (\category -> let (Category c) = category in (c.name, category)) categories
    dict' = Dict.fromList   <| List.map (\category -> let (Category c) = category in (c.id, category))   categories

    findParent mp = flip Maybe.map mp <| \p -> case p of
      Left pidx -> mapDefault (Dict.get pidx dict') p Right
      Right _ -> p

    castegories' = Dict.map (\_ category ->
      let (Category c) = category
      in Category { c
      | parent = findParent c.parent
      , childs = childs category categories
    }) dict
  in
    Response <| noFx {state | categories = castegories'}

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

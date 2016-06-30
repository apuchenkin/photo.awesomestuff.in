module App.Actions exposing (..)

import Http
import Random
import Process
import Window
import Time exposing (Time)
import Either exposing (Either (..))
import Dict exposing (Dict)
import Json.Decode  as Json exposing ((:=))
import Task exposing (Task)
import Router.Types exposing (WithRouter, Action, Response (..), Router)
import Router.Helpers exposing (noFx, chainAction, doNothing, combineActions)

import App.Model exposing (..)
import App.Config exposing (config)
import App.Routes as Routes exposing (Route)
import App.Locale as Locale exposing (Locale)
import App.Ports exposing (portCmd, portDelayCmd, execute)
import Service.Photo exposing (refinePhotos)

mapDefault : Maybe a -> b -> (a -> b) -> b
mapDefault maybe default map = Maybe.withDefault default <| Maybe.map map maybe

succ : number -> number
succ a = a + 1

pred : number -> number
pred a = a - 1

onTransition : Router Route State -> Maybe from -> Maybe to -> Action State
onTransition router _ to = case to of
  Nothing -> fallbackAction router
  Just route -> combineActions [
      resetMeta
    , resetLoading
    , createLinks router
    , \state -> Response (state, portCmd <| App.Ports.meta state.meta) -- send meta to port
    , \state -> Response <| noFx {state | defer = Deferred (App.Ports.transition <| Maybe.map toString to) :: state.defer}
    ]

type TransitionType = In | Out

catMaybes : List (Maybe a) -> List a
catMaybes = List.filterMap identity

transitionAction : Bool -> TransitionType -> Action State
transitionAction transitionState transitionType state =
  let
    -- _ = Debug.log "transitionAction" transitionState
    transition = state.transition
    transition' = case transitionType of
      In -> {transition | transitionIn = transitionState}
      Out -> {transition | transitionOut = transitionState}
  in
    Response <| noFx { state | transition = transition' }

withTransition : TransitionType -> Maybe (Action State) -> Action State
withTransition transitionType action state =
  let
    (Response (state', _)) = transitionAction True transitionType state
    effects = execute <| Process.sleep config.transition `Task.andThen` (\_ -> Task.succeed <|
      combineActions <| catMaybes <| [action, Just <| transitionAction False transitionType]
      )
  in
    Response <| (state', effects)

isLoading : State -> Bool
isLoading state = state.isLoading

resetLoading : Action State
resetLoading state = withTransition In Nothing { state | isLoading = False }

startLoading : Action State
startLoading state = withTransition In Nothing { state | isLoading = True }

stopLoading : Action State
stopLoading state = withTransition In Nothing { state | isLoading = False }

fallbackAction : Router Route State -> Action State
fallbackAction router state = let _ = Debug.log "nf" () in router.redirect (Routes.NotFound, state.router.params) state

setTitle : Maybe String -> Action State
setTitle title state =
  let
    meta = state.meta
    title' = Maybe.withDefault config.title <| Maybe.map (\t -> Locale.i18n state.locale "TITLE" [t]) title
  in Response <| noFx {state | meta = {meta | title = title'}}

setDescription : Maybe String -> Action State
setDescription desc state =
  let
    meta = state.meta
    desc' = Maybe.withDefault (Locale.i18n state.locale "META.DESCRIPTION" []) desc
  in Response <| noFx {state | meta = {meta | description = desc'}}

resetMeta : Action State
resetMeta = setTitle Nothing `chainAction` setDescription Nothing

setMetaFromCategory : Category -> Action State
setMetaFromCategory (Category c) = setTitle (Just c.title)
    `chainAction`
    setDescription (Maybe.withDefault c.description <| Maybe.map Just c.shortDescription)

setMetaFromPhoto : Photo -> Action State
setMetaFromPhoto photo = setTitle photo.caption
    `chainAction`
    (\state -> setDescription (Maybe.map2 (\author caption -> Locale.i18n state.locale "META.DESCRIPTION.PHOTO" [author.name, caption]) photo.author photo.caption) state)

setTime : Time -> Action State
setTime time state = Response <| noFx {state | time = time}

setSize : Window.Size -> Action State
setSize dims state = Response <| noFx {state | window = dims}

tick : Time -> Action State
tick frame state =
  let
    -- _ = Debug.log "tick" frame
    cmd = List.map (\(Deferred cmd) -> cmd) state.defer
  in Response <| {state | defer = []} ! cmd

setLocale : Locale -> Action State
setLocale locale state = Response <| noFx {state | locale = locale}

createLinks : Router Route State -> Action State
createLinks router state =
  let
    meta = state.meta
    links = flip Maybe.map state.router.route <| \ route ->
      ("x-default", config.hostname ++ router.buildUrl (route, Dict.remove "locale" state.router.params))
      :: (flip List.map Locale.locales
      <| \locale -> (Locale.toString locale, config.hostname ++ router.buildUrl (route, Dict.insert "locale" (Locale.toString locale) state.router.params))
      )

  in Response <| noFx {state | meta = {meta | links = Maybe.withDefault [] links}}

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
    (Response (state', _)) = startLoading state
    fetch = Task.toMaybe <| getRequest decodeCategories (config.apiEndpoint ++ "/category") state'
    task = fetch `Task.andThen` \mcategories ->
      let
        categories = Maybe.withDefault [] mcategories
        update = updateCategories categories
        categoryParam =  case Dict.get "subcategory" state.router.params of
            Nothing -> Dict.get "category" state.router.params
            c -> c
        action = Maybe.withDefault (update `chainAction` stopLoading) <| flip Maybe.map categoryParam <| \category ->
          combineActions <| catMaybes <| [
            Just update
          , Just <| checkCategory router category
          , Just <| loadPhotos router
          , Maybe.map setMetaFromCategory (getCategory state)
          ]

      in Task.succeed <| action

  in Response ({state' | categories = Dict.empty}, execute task)

checkCategory : Router Route State -> String -> Action State
checkCategory router category state =
    case Dict.get category state.categories of
      Nothing -> router.redirect (Routes.Home, Dict.empty) state
      _ -> (\state -> let _ = Debug.log "checkCategory" in Response <| noFx state) state

loadPhotos : Router Route State -> Action State
loadPhotos router state =
  let
    task = case Dict.isEmpty state.categories of
      True   -> Nothing
      False  ->
        let
          category = getCategory state
          default = Task.succeed <| router.redirect (Routes.NotFound, Dict.empty) `chainAction` stopLoading
          updateTask photos = Task.succeed <| (updatePhotos <| Maybe.withDefault [] photos) `chainAction` stopLoading
          fetchTask (Category c) = Task.toMaybe <| getRequest (decodePhotos category) (config.apiEndpoint ++ "/category/" ++ toString c.id ++ "/photo") state
          task = flip Maybe.map category
            <| \c ->  fetchTask c `Task.andThen` updateTask

        in Just <| Maybe.withDefault default task
    (Response (state', _)) = startLoading state

  in Response (mapDefault task state (always state'), mapDefault task Cmd.none execute)

loadPhoto : Action State
loadPhoto state =
  let
    photoId = Dict.get "photo" state.router.params
    task = flip Maybe.map photoId <| \pid ->
      let fetch = Task.toMaybe <| getRequest (decodePhoto Nothing) (config.apiEndpoint ++ "/photo/" ++ pid) state
      in fetch `Task.andThen` \photo -> Task.succeed <| stopLoading `chainAction` updatePhoto photo
    (Response (state', effects)) = startLoading {state | photo = Nothing}

  in Response (state', Cmd.batch [effects, mapDefault task Cmd.none execute])

updatePhotos : List Photo -> Action State
updatePhotos photos state =
  let
    seed = Random.initialSeed <| floor <| Time.inSeconds state.time
    photos' = refinePhotos seed photos
    -- _ = Debug.log "updatePhotos" ()
  in Response <| noFx {state | photos = photos', defer = Deferred (portCmd <| App.Ports.photos True) :: state.defer}

updatePhoto : Maybe Photo -> Action State
updatePhoto photo = combineActions <| catMaybes <| [
    Just <| \state -> Response <| noFx {state | photo = photo}
  , Maybe.map setMetaFromPhoto photo
  ]

updateCategories : List Category -> Action State
updateCategories categories state =
  let
    idMap = Dict.fromList   <| List.map (\category -> let (Category c) = category in (c.id, category))   categories

    findParent mp = flip Maybe.map mp <| \p -> case p of
      Left pidx -> mapDefault (Dict.get pidx idMap) p Right
      Right _ -> p

    categories' = List.map (\(Category c) -> Category { c | parent = findParent c.parent }) categories
    dict = Dict.fromList <| List.map (\category ->
      let (Category c) = category
      in (c.name, Category { c | childs = childs category categories'})) categories'
  in
    Response <| noFx {state | categories = dict}

resolveLocale : Router Route State -> Action State
resolveLocale router state =
  let
    locale = Dict.get "locale" state.router.params
    route = Maybe.withDefault Routes.Home state.router.route
    params = Dict.fromList [("locale", Locale.toString state.locale)]
  in case locale of
    Nothing -> router.redirect (route, Dict.union params state.router.params) state
    Just l  -> Response <| noFx {state | locale = Locale.fromString l}

module Lib.Functions where

-- import History
import Dict
import List.Extra
import MultiwayTreeUtil
import Effects          exposing (Effects)
import Html             exposing (Html)
import Lib.Matcher
import Lib.Types    exposing (..)
import Lib.Helpers  exposing (..)
import Lib.Mailbox      exposing (..)

--------------------------------------------------------------------------------------
-- private router functions
--------------------------------------------------------------------------------------

{-| @Private -}
runHandlers : List (Handler state) -> List (Action state)
runHandlers handlers =
  let
    run actions state = Response <| List.foldl runAction (noFx state) actions
  in List.map run <| List.map .actions handlers

{-| @Private -}
runAction : Action state -> (state, ActionEffects state) -> (state, ActionEffects state)
runAction action (state, effects) =
    let
        (Response (state', effects')) = action state
    in
        (state', Effects.batch [effects, effects'])

{-| @Private -}
prepareCache : (WithRouter route state) -> RouterConfig route (WithRouter route state) -> (WithRouter route state)
prepareCache state config =
  let
    router = state.router
    routes = List.concat <| List.map MultiwayTreeUtil.flatten config.routes
    urls = flip List.map routes <| \r -> (toString r, Lib.Matcher.composeRawUrl (.segment << config.config) config.routes r)
    urls' = List.map (.segment << config.config) routes
    unwraps = flip List.map (urls' ++ List.map snd urls) <| \url -> (url, Lib.Matcher.unwrap url)
    rawUrl = Dict.fromList urls
    unwrap  = Dict.fromList unwraps
    traverses = flip List.map routes (\route -> (toString route, Lib.Matcher.getPath route config.routes))
    traverse = Dict.fromList traverses
    cache = {rawUrl = rawUrl, unwrap = unwrap, traverse = traverse}
  in {state | router = {router | cache = cache}}

{-| @Private -}
render : Router route (WithRouter route state) -> (WithRouter route state) -> Html
render router state =
    let
      -- _ = Debug.log "render" state.router
      route     = state.router.route
      handlers  = Maybe.withDefault [] <| Maybe.map (\to -> getHandlers router state.router Nothing (to, Dict.empty)) route
      views     = List.map .view handlers
      html      = List.foldr (\view parsed -> view address state parsed) Nothing views
    in Maybe.withDefault (Html.text "error") html

{-| @Private -}
setUrl : Router route (WithRouter route state) -> RouterState route -> String -> Action (WithRouter route state)
setUrl router state url =
  let
  _ = Debug.log "setUrl" url
  (Router r) = router
  in case (matchRoute r.config state url) of
    Nothing               -> setRoute router r.config.fallback
    Just route            -> setRoute router route

{-| @Private -}
setRoute : Router route (WithRouter route state) -> Route route -> Action (WithRouter route state)
setRoute router route state =
  let
    -- _ = Debug.log "setRoute" route
    rs = state.router
    (toRoute, toParams) = route
    from  = Maybe.map (\r -> (r, rs.params)) rs.route
    state' = { state | router = { rs | route = Just toRoute, params = toParams }}
  in
    transition router from route state'

{-| @Private -}
transition : Router route (WithRouter route state) -> Transition route (WithRouter route state)
transition router from to state =
  let
    -- _ = Debug.log "transition: from" (from, to)
    handlers = getHandlers router state.router from to
    actions  = runHandlers handlers
  in  Response <| List.foldl runAction (noFx state) actions

{-| @Private -}
getHandlers : Router route state -> RouterState route -> Maybe (Route route) -> Route route -> List (Handler state)
getHandlers router state from to =
  let
    (Router r) = router
    fromRoute = Maybe.map fst from
    fromParams = Maybe.withDefault Dict.empty <| Maybe.map snd from
    toRoute = fst to
    toParams = snd to

    fromPath = Maybe.withDefault []
     <| flip Maybe.map fromRoute
     <| \f -> case Dict.get (toString f) state.cache.traverse of
      Just path -> path
      Nothing   -> Lib.Matcher.getPath f r.config.routes
    toPath = case Dict.get (toString toRoute) state.cache.traverse of
     Just path -> path
     Nothing   -> Lib.Matcher.getPath toRoute r.config.routes
    path = List.map2 (,) fromPath toPath

    fromPath' = Lib.Matcher.mapParams (.segment << r.config.config) fromPath fromParams
    toPath'   = Lib.Matcher.mapParams (.segment << r.config.config) toPath    toParams

    commons = List.length
      <| List.Extra.takeWhile (uncurry (==))
      <| List.map2 (,) fromPath' toPath'

    routes = List.drop commons toPath

  in List.map ((\h -> h router) << .handler << r.config.config) <| routes

{-| @Private -}
matchRoute : RouterConfig route state -> RouterState route -> String -> Maybe (Route route)
matchRoute config state url =
  let
    rawRoute route = case Dict.get (.segment <| config.config route) state.cache.unwrap of
      Just value -> (value, .constraints <| config.config route)
      Nothing -> (Lib.Matcher.unwrap <| .segment <| config.config route, .constraints <| config.config route)
  in
    Lib.Matcher.matchRaw rawRoute config.routes url

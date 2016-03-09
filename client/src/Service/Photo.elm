module Service.Photo where

import Dict
import Random
import List.Extra exposing ((!!))
import App.Model exposing (Photo)

photoMode : Float -> Photo -> Random.Seed -> (Int, Random.Seed )
photoMode avg photo seed =
  let
    v = toFloat photo.views
    std = sqrt <| (v - avg) ^ 2
    norm  = List.map ((*) (floor avg)) [32,16,4,1]
    norm' = List.map (\r -> floor <| r * (std * v) / avg) [1,2,3,4]
    prob = List.map2 (+) norm norm'
    prob' = List.map (\p -> p // (Maybe.withDefault 1 <| List.minimum prob)) prob
    probList = List.concat <| List.map2 (\m p -> List.repeat p m) [1,2,3,4] prob'
    gen = Random.int 0 (List.length probList - 1)
    (r, seed') = Random.generate gen seed

    mode = Maybe.withDefault 1 <| probList !! r
  in (mode, seed')

{-|
  Reduces list of photos by removing photo dublicates
-}
refinePhotos : Random.Seed -> List Photo -> List Photo
refinePhotos seed photos =
  let
    orderedPhotos = List.map2 (,) [1 .. List.length photos] photos
    groups = List.foldl (\op dict ->
      let
        p = snd op
        idx = Maybe.withDefault 0 p.group
        update v = Just <| op :: (Maybe.withDefault [] v)
      in
        Dict.update idx update dict) Dict.empty orderedPhotos

    photos' = Maybe.withDefault [] <| Dict.get 0 groups
    groups' = List.filterMap identity <| snd <| Dict.foldl (\_ list (s, res) ->
      let
        gen = Random.int 0 (List.length list - 1)
        (r, s') = Random.generate gen s
      in (s', list !! r :: res)) (seed, []) <| Dict.remove 0 groups

  in List.map snd <| List.sortBy fst <| groups' ++ photos'

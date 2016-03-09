module Service.Photo (refinePhotos, remapPhotos) where

import Dict
import Random
import List.Extra exposing ((!!))
import App.Model exposing (Photo)
import App.Config exposing (config)

type PhotoMode = A | B | C | D

sizes : (Int, Int, Int, Int)
sizes = (
    config.brickWidth
  , config.brickWidth * 2 + config.gutter
  , config.brickWidth * 3 + config.gutter * 2
  , config.brickWidth * 4 + config.gutter * 3
  )

dsmap : PhotoMode -> Float -> Bool -> ( Int, Int )
dsmap mode ratio isHorisontal =
  let (s1,s2,s3,s4) = sizes
  in case mode of
  A -> (if ratio >= 2 then s2 else s1, s1)
  B -> if isHorisontal then (if ratio >= 3 then s3 else s2, s1) else (s1, s2)
  C -> if ratio >= 4 then (s4, s1) else (if ratio >= 2 then s3 else s2, s2)
  D -> if isHorisontal then (if ratio >= 2 then s4 else s3, s2) else (s2, s3)

remapPhotos : Random.Seed -> List Photo -> List Photo
remapPhotos seed photos =
  let
    views = List.map .views photos
    avg = (toFloat <| List.sum views) / (toFloat <| List.length views)
  in List.reverse <| fst <| List.foldl (\p (acc, s) -> let (mode, s') = remapPhoto s avg p in (mode :: acc, s')) ([], seed) photos

remapPhoto : Random.Seed -> Float -> Photo -> (Photo, Random.Seed)
remapPhoto seed avg photo =
  let
    v = toFloat photo.views
    std = sqrt <| (v - avg) ^ 2
    norm  = List.map ((*) (floor avg)) [32,16,4,1]
    norm' = List.map (\r -> floor <| r * (std * v) / avg) [1,2,3,4]
    prob = List.map2 (+) norm norm'
    prob' = List.map (\p -> p // (Maybe.withDefault 1 <| List.minimum prob)) prob
    probList = List.concat <| List.map2 (\m p -> List.repeat p m) [A,B,C,D] prob'
    gen = Random.int 0 (List.length probList - 1)
    (r, seed') = Random.generate gen seed
    mode = Maybe.withDefault A <| probList !! r
    isHorisontal = (photo.width > photo.height)
    ratio = toFloat photo.width / toFloat photo.height
    inc = if ratio >= 1 then ratio else 1 / ratio
    (w,h) = dsmap mode ratio isHorisontal
  in ({ photo | width = w, height = h}, seed')

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

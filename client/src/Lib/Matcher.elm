module Lib.Matcher
 -- (match, combineParams, buildUrl)
 (..) -- test
  where

import Regex
import String
import Dict

import Util.Util      exposing (treeLookup, traverse)
import MultiwayTree   exposing (Tree (..), Forest, datum, children)
import List.Extra
import Combine        exposing (Parser, many1, parse, many, while, between, end, rec, manyTill)
import Combine.Char   exposing (char, noneOf, anyChar)
import Combine.Infix  exposing ((<$>), (*>), (<*), (<*>), (<|>))

import Lib.Types    exposing (GetRouteConfig, RouteParams, Route)
import Lib.Helpers  exposing (singleton)

type alias URL    = String
type alias RawURL = String

paramChar : Char
paramChar = ':'

-- left delimiter
ld : Char
ld = '['

-- right delimiter
rd : Char
rd = ']'

stringParser : Parser String
stringParser = String.fromList <$> many1 (noneOf [ '/', paramChar, '#', '?' ])

paramParser : Parser String
paramParser = char paramChar *> stringParser

paramsParser : Parser (List String)
paramsParser = many <| while ((/=) paramChar) *> paramParser <* while ((/=) paramChar)

getParams : String -> List String
getParams string = case fst <| parse paramsParser string of
  Err _     -> Debug.crash "getParams : String -> List String"
  Ok param  -> param

combineParams : RouteParams -> Route route -> Route route
combineParams dict (route, params) = (route, Dict.union params dict)

cache : List String
cache = []

unwrap : String -> List String
unwrap raw =
  let
    cache = "s" :: cache
    regex   = Regex.regex "^(.*)\\[([^\\]\\[]+)\\](.*)$"
    matches = Regex.find (Regex.AtMost 1) regex raw
    result = case matches of
      []         -> [raw]
      [match]    -> case match.submatches of
        [Just a, Just b, Just c] -> List.concat <| List.map unwrap [a ++ b ++ c, a ++ c]
        _ -> [raw]
      _ -> Debug.crash "result = case matches of _"

  in List.reverse <| List.sortBy String.length <| List.Extra.dropDuplicates <| result

-- TODO: Regex free?
-- TODO: Perfomance?
parseUrlParams : RawURL -> URL -> (Result (List String) RouteParams, String)
parseUrlParams raw url =
  let
    params = getParams raw
    strings = case params of
      [] -> [raw]
      p  -> Regex.split Regex.All (Regex.regex <| String.join "|" <| List.map (String.cons paramChar) p) raw

    sringParsers = List.map Combine.string        strings
    paramParsers = List.map (always stringParser) params

    last = case List.Extra.last sringParsers of
      Nothing -> Debug.crash "List.Extra.last sParsers"
      Just v  -> v

    parsers = List.map2 (\p1 p2 -> p1 *> p2) sringParsers paramParsers
    parser = (List.foldr (\p pacc -> p `Combine.andThen` (\r -> (++) r <$> pacc))
       (singleton <$> last)
       (List.map (Combine.map singleton) parsers)
       ) -- <* Combine.end

    (result, context) = Combine.parse parser url
    zipValues values = Dict.fromList <| List.map2 (,) params values
  in (Result.map zipValues result, context.input)

-- TODO: cache unwrap?
match : (route -> RawURL) -> Forest route -> URL -> Maybe (Route route)
match rawRoute forest url = List.head <| List.filterMap (\tree ->
    let
      raw = rawRoute <| datum tree
      raws = unwrap raw

    in List.head <| List.filterMap (\pattern -> let (result, url') = parseUrlParams pattern url
       in case result of
        Err _       -> Nothing
        Ok  dict    -> case String.isEmpty url' of
            True  -> Just (datum tree, dict)
            False -> case children tree of
              []        -> Nothing
              forest    ->
                let child = match rawRoute forest url'
                in Maybe.map (combineParams dict) child
      ) raws
    ) forest

-- decompose Route to string
-- TODO: cache unwrap?
buildUrl : (route -> RawURL) -> Forest route -> Route route -> URL
buildUrl rawRoute tree (route, params) =
  let
    -- _ = Debug.log "buildUrl" (route, params)
    zipper   = List.head <| List.filterMap (\r -> treeLookup route (r, [])) tree
    path = Maybe.withDefault [] <| Maybe.map traverse zipper
    segments = List.map rawRoute path
    rawUrl = List.foldl (flip (++)) "" segments
    raws = unwrap rawUrl
    urls = List.map (\raw -> Dict.foldl (\param value string -> Regex.replace
            (Regex.AtMost 1)
            (Regex.regex <| paramChar `String.cons` param)
            (always value)
            string
          ) raw params
      ) raws
    urls' = List.filter (not << String.contains (String.fromChar paramChar)) urls

  in case List.head urls' of
    Nothing -> Debug.crash "not enough params to build URL"
    Just url -> url

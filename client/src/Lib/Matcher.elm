module Lib.Matcher (match) where

import Regex
import String
import Dict

import MultiwayTree exposing (Tree (..), Forest, datum, children)
import List.Extra
import Combine        exposing (Parser, many1, parse, many, while)
import Combine.Char   exposing (char, noneOf)
import Combine.Infix  exposing ((<$>), (*>), (<*))

import Lib.Types    exposing (GetRouteConfig, RouteParams)
import Lib.Helpers  exposing (singleton)

paramChar : Char
paramChar = ':'

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

combineParams : RouteParams -> (route, RouteParams) -> (route, RouteParams)
combineParams dict (route, params) = (route, Dict.union dict params)

match : GetRouteConfig route state -> Tree route -> String -> Maybe (route, RouteParams)
match getConfig tree url =
  let
    raw = .url << getConfig <| datum tree

    params = getParams raw

    strings = case params of
      [] -> [raw]
      p  -> Regex.split Regex.All (Regex.regex <| String.join "|" <| List.map (String.cons paramChar) p) raw

    -- _ = Debug.log "url" url
    -- _ = Debug.log "params" <| params
    -- _ = Debug.log "strings" <| strings

    sringParsers = List.map Combine.string        strings
    paramParsers = List.map (always stringParser) params

    last = case List.Extra.last sringParsers of
      Nothing -> Debug.crash "List.Extra.last sParsers"
      Just v  -> v

    parsers = List.map2 (\p1 p2 -> p1 *> p2) sringParsers paramParsers

    ----------------------------------------------------------------

    parser = (List.foldr (\p pacc -> p `Combine.andThen` (\r -> (++) r <$> pacc))
       (singleton <$> last)
       (List.map (Combine.map singleton) parsers)
       ) -- <* Combine.end

    result = Combine.parse parser url

    -- _ = Debug.log "!" result
    url' = .input <| snd result
  in case (fst result) of
    Err _       -> Nothing
    Ok  values  ->
      let dict = Dict.fromList <| List.map2 (,) params values
      in case String.isEmpty url' of
        True  -> Just (datum tree, dict)
        False -> case children tree of
          []        -> Nothing
          forest    ->
            let child = List.head <| List.filterMap (flip (match getConfig) url') forest
            in Maybe.map (combineParams dict) child

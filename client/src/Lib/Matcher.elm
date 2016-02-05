module Lib.Matcher
 -- (match, combineParams, buildUrl)
 (..) -- test
  where

import Regex
import String
import Dict

import Util.Util      exposing (treeLookup, traverseUp)
import MultiwayTree   exposing (Tree (..), Forest, datum, children)
import List.Extra
import Combine        exposing (Parser, many1, parse, many, while, between, end)
import Combine.Char   exposing (char, noneOf)
import Combine.Infix  exposing ((<$>), (*>), (<*), (<|>))

import Lib.Types    exposing (GetRouteConfig, RouteParams, Route)
import Lib.Helpers  exposing (singleton)

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
combineParams dict (route, params) = (route, Dict.union dict params)

{-| Parse something between square brackets `[]`. -}
brackets : Parser res -> Parser res
brackets = between (char ld) (char rd)

unwrap : String -> List String
unwrap url =
  let
    ch = noneOf [ ld, rd ]
    parser = brackets (many1 ch) <|> wrappedParser
    wrappedParser = while ((/=) ld) *> parser <* many ch <* end
    result = parse (wrappedParser) url
    _ = Debug.log "result" result
  in [url]

match : GetRouteConfig route state -> Tree route -> String -> Maybe (Route route)
match getConfig tree url =
  let
    raw = .url << getConfig <| datum tree
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

    result = Combine.parse parser url
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

-- decompose Route to string
buildUrl : GetRouteConfig route state -> Forest route -> Route route -> String
buildUrl getConfig tree (route, params) =
  let
    -- _ = Debug.log "buildUrl" (route, params)
    zipper   = List.head <| List.filterMap (\r -> treeLookup route (r, [])) tree
    traverse = Maybe.withDefault [] <| Maybe.map traverseUp zipper
    segments = List.map (.url << getConfig) traverse
    rawUrl = List.foldl (flip (++)) "" segments
    url = Dict.foldl (\param value string -> Regex.replace
      (Regex.AtMost 1)
      (Regex.regex <| paramChar `String.cons` param)
      (always value)
      string
    ) rawUrl params

  in url

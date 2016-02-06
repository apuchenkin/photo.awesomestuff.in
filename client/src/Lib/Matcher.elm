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
combineParams dict (route, params) = (route, Dict.union dict params)

-- TODO: regex version of function
-- TODO: Perfomance?
unwrap : String -> List String
unwrap raw =
  let
    _ = Debug.log "unwrap" raw
    ch : Parser Char
    ch = noneOf [ld, rd]

    space : Parser String
    space = String.fromList <$> many ch

    plain : Parser String
    plain  = String.fromList <$> (many1 ch)
    -- plain  = Combine.regex "^[^\\[\\]]+"

    brackets : Parser a -> Parser a
    brackets = between (char ld) (char rd)

    -- nested : Parser (List String)
    -- nested = List.concat <$> (many1 <| List.append <$>  (((List.append << singleton) <$> space) <*> (brackets parser)) <*> (singleton <$> space))
    --
    -- parser :  Parser (List String)
    -- parser = rec (\_ -> nested <|> (singleton <$> plain))

    nested : Parser (Forest String)
    nested = List.concat <$> (many1 <|
       (List.append << singleton)
       <$> ((Tree <$> space) <*> brackets parser)
       <*> ((\r -> case r of
          "" -> []
          _  -> singleton (Tree r [])) <$> space)
    )

    parser :  Parser (Forest String)
    parser = rec (\_ -> nested <|> ((singleton << (flip Tree [])) <$> plain))
    -- parser   = Combine.or (singleton <$> plain <* end) <| between space space <| List.concat <$> many (Combine.brackets (between space space <| many (Combine.brackets plain))) -- etalon

    result = parse (parser <* end) raw
    -- _ = Debug.log "result" result

    flatten : Forest String -> List String
    flatten forest =
      let
        opts = List.map (\tree ->
          let ch = (flatten <| children tree)
          in datum tree :: List.map ((++) (datum tree)) ch) forest

        -- _ = Debug.log "flatten" opts
      in List.Extra.dropDuplicates <| List.foldl (\o acc-> List.concat <| List.map (\a -> List.map (flip (++) a) acc) o) [""] opts

    variations = case fst result of
      Ok forest -> flatten forest
      Err _     -> [Debug.crash "cannot parse provided string: \"" ++ raw ++ "\""]

    in List.reverse <| List.sortBy String.length variations

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

match : (route -> RawURL) -> Tree route -> URL -> Maybe (Route route)
match rawRoute tree url =
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
              let child = List.head <| List.filterMap (flip (match rawRoute) url') forest
              in Maybe.map (combineParams dict) child
    ) raws

-- decompose Route to string
buildUrl : (route -> RawURL) -> Forest route -> Route route -> URL
buildUrl rawRoute tree (route, params) =
  let
    -- _ = Debug.log "buildUrl" (route, params)
    zipper   = List.head <| List.filterMap (\r -> treeLookup route (r, [])) tree
    traverse = Maybe.withDefault [] <| Maybe.map traverseUp zipper
    segments = List.map rawRoute traverse
    rawUrl = List.foldl (flip (++)) "" segments
    url = Dict.foldl (\param value string -> Regex.replace
      (Regex.AtMost 1)
      (Regex.regex <| paramChar `String.cons` param)
      (always value)
      string
    ) rawUrl params

  in url

module Util.Util where

import Maybe
import MultiwayTree as Tree exposing (Tree (..), Forest, datum)
import MultiwayTreeZipper exposing (Zipper, Context (..), goToChild, goUp)

leftContext : Context a -> List (Tree a)
leftContext (Context _ left _) = left

treeToList : Tree a -> List a
treeToList tree =
  let
    tail = List.concatMap treeToList (Tree.children tree)
  in (Tree.datum tree) :: tail

traverse : Zipper a -> List a
traverse zipper = traverseFrom zipper Nothing

traverseFrom : Zipper a -> Maybe a -> List a
traverseFrom zipper value =
  let value' = datum <| fst zipper
  in case value == Just value' of
      True -> []
      False -> case goUp zipper of
        Nothing      -> [value']
        Just zipper' -> traverseFrom zipper' value ++ [value']

zipper : Tree a -> Zipper a
zipper tree = (tree, [])

forestLookup : a -> Forest a -> Maybe (Zipper a)
forestLookup value forest = List.head <| List.filterMap (\tree -> treeLookup value (zipper tree)) forest

treeLookup : a -> Zipper a -> Maybe (Zipper a)
treeLookup value z =
  let
    tree = fst z
    -- _ = Debug.log "treeLookup" value
    -- _ = Debug.log "treeLookup" <| Tree.datum tree
    -- ++ "|" ++ ()
    unsafeFromJust : Maybe a -> a
    unsafeFromJust mb = case mb of
      Just v -> v
      Nothing -> Debug.crash "unsafeFromJust"

  in case Tree.datum tree == value of
    True -> Just z
    False -> case Tree.children tree of
      []     -> Nothing
      childs -> List.head <| List.filterMap (\idx -> treeLookup value (unsafeFromJust <| flip goToChild z idx)) [0.. List.length childs - 1]

-- lowest common ancestor
-- disjoint tree set
lca : Forest a -> a -> a -> Maybe a
lca forest a b =
  let
    -- _ = Debug.log "getHandlers" (from, to)
    zipperFrom = List.head <| List.filterMap (\r -> treeLookup a (r, [])) forest
    zipperTo =   List.head <| List.filterMap (\r -> treeLookup b (r, [])) forest

    traverseFrom = Maybe.withDefault [] <| Maybe.map traverse zipperFrom
    traverseTo   = Maybe.withDefault [] <| Maybe.map traverse zipperTo

    routes = case traverseTo of
      [] -> []
      _  -> case (List.head traverseTo) == (List.head traverseFrom) of
        False -> []
        True  -> List.filter (\(f,t) -> f == t) <| List.map2 (,) traverseFrom traverseTo

  in List.head <| List.drop (List.length routes - 1) traverseTo

-- path from node a to node b in the forest
getPath : Forest a -> Maybe a -> a -> List a
getPath forest from to = Maybe.withDefault []
  <| flip Maybe.map (List.head <| List.filterMap (\tree -> treeLookup to (tree, [])) forest)
  <| \zipper ->
    let from' = from `Maybe.andThen` (\from' -> lca forest from' to)
    in traverseFrom zipper from'

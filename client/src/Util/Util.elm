module Util.Util where

import MultiwayTree as Tree exposing (Tree (..), Forest, datum)
import MultiwayTreeZipper exposing (Zipper, Context (..), goToChild, goUp)

leftContext : Context a -> List (Tree a)
leftContext (Context _ left _) = left

treeToList : Tree a -> List a
treeToList tree =
  let
    tail = List.concatMap treeToList (Tree.children tree)
  in (Tree.datum tree) :: tail

traverseUp : Zipper a -> List a
traverseUp zipper =
  let value = [datum <| fst zipper]
  in case goUp zipper of
    Nothing      -> value
    Just zipper' -> traverseUp zipper' ++ value


treeLookup : a -> Zipper a -> Maybe (Zipper a)
treeLookup value z =
  let
    tree = fst z
    _ = Debug.log "treeLookup" value
    _ = Debug.log "treeLookup" <| Tree.datum tree
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

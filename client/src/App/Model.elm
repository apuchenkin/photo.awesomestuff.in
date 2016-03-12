module App.Model where

import Dict exposing (Dict)
import Date exposing (Date)
import Time exposing (Time)
import Either exposing (Either (..))
import Json.Decode  as Json exposing ((:=))
import Router.Types exposing (WithRouter, Action, Response (..), Router)

import App.Locale as Locale exposing (Locale)
import App.Routes as Routes exposing (Route)

(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) = Maybe.andThen

type alias Meta = {
    title: String,
    description: String,
    links: List (String, String)
  }

type alias State = WithRouter Route {
    meta: Meta
  , locale: Locale
  , categories: Dict String Category
  , photos: List Photo
  , photo: Maybe Photo
  , isLoading: Int
  , time: Time
  , window: (Int, Int)
  }

type Category = Category {
    id: Int
  , name: String
  , title: String
  , image: Maybe String
  , date: Maybe Date
  , parent: Maybe (Either Int Category)
  , description: Maybe String
  , shortDescription: Maybe String
  , childs: List Category
  }

-- Category constuctor
category : Int -> String -> String -> Maybe String -> Maybe String -> Maybe (Either Int Category) -> Maybe String -> Maybe String -> Category
category id name title image date parent desc shortDesc = Category {
    id = id
  , name = name
  , title = title
  , image = image
  , date  = date `Maybe.andThen` \d -> Result.toMaybe (Date.fromString d)
  , parent = parent
  , description = desc
  , shortDescription = shortDesc
  , childs = []
  }

decodeCategories : Json.Decoder (List Category)
decodeCategories = Json.list <| Json.object8 category
  ("id"     := Json.int)
  ("name"   := Json.string)
  ("title"  := Json.string)
  (Json.maybe ("image" := Json.string))
  (Json.maybe ("date" := Json.string))
  (Json.maybe ("parent" := Json.map Left Json.int))
  (Json.maybe ("description" := Json.string))
  (Json.maybe ("short_description" := Json.string))

-- todo: persist child in categories
childs : Category -> List Category -> List Category
childs category categories =
  let (Category pc) = category
  in flip List.filter categories <| \(Category c) ->
  Maybe.withDefault False <| c.parent &> \p -> Just <| Either.elim ((==) pc.id) ((==) category) p

type alias Photo = {
    id: Int
  , src: String
  , width: Int
  , height: Int
  , ratio: Float
  , views: Int
  , group: Maybe Int
  , caption: Maybe String
  , author: Maybe Author
  }

-- Category constuctor
photo : Int -> String -> Int -> Int -> Int -> Maybe Int -> Maybe String -> Maybe Author -> Photo
photo id src width height views group caption author =
  let ratio = (toFloat width) / (toFloat height)
  in {
    id = id
  , src = src
  , width = width
  , height = height
  , ratio = ratio
  , views = views
  , group = group
  , caption = caption
  , author = author
  }

decodePhoto : Json.Decoder Photo
decodePhoto = Json.object8 photo
  ("id"     := Json.int)
  ("src"    := Json.string)
  ("width"  := Json.int)
  ("height" := Json.int)
  ("views"  := Json.int)
  (Json.maybe ("group"   := Json.int))
  (Json.maybe ("caption" := Json.string))
  (Json.maybe ("author"  := Json.object1 Author ("name" := Json.string)))

decodePhotos : Json.Decoder (List Photo)
decodePhotos = Json.list decodePhoto

type alias Author = {
    name: String
  }

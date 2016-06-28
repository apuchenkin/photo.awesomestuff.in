module App.Routes exposing (..)

type Route = Locale | Home | NotFound | Static String | Category | Photo

routes : List Route
routes = [Locale, NotFound, Static "about", Static "contacts", Home, Category, Photo]

module Handler.Static exposing (..)

import Html exposing (Html, text, span)
import Html.Lazy exposing (lazy, lazy2)
import Html.Attributes as Attr
import Router.Types exposing (Router, Action)

import App.Locale as Locale exposing (Locale)
import App.Routes as Routes exposing (Route)
import App.Model exposing (State)
import Handler.Widgets exposing (homeLink)

aboutWidget : Locale -> Html (Action State)
aboutWidget = lazy <| \locale -> Html.div [Attr.class "about"] [
    Html.p [] [Html.text <| Locale.i18n locale <| Locale.About Locale.About1]
  , Html.a [Attr.target "_blank", Attr.href <| Locale.i18n locale <| Locale.Form Locale.Bugreport] [Html.text <| Locale.i18n locale <| Locale.Form Locale.Bugreport]
  , Html.p [] [Html.text <| Locale.i18n locale <| Locale.About Locale.About2]
  , Html.a [Attr.target "_blank", Attr.href <| Locale.i18n locale <| Locale.Form Locale.Feedback] [Html.text <| Locale.i18n locale <| Locale.Form Locale.Feedback]
  , Html.p [] [Html.text <| Locale.i18n locale <| Locale.About Locale.About3]
  ]

contactsWidget : Locale -> Html (Action State)
contactsWidget = lazy <| \locale -> Html.div [Attr.class "contacts"] [
    Html.h3 [] [Html.text <| Locale.i18n locale <| Locale.Contacts Locale.Authors]
  , Html.div [Attr.class "details"] [
      Html.b [] [Html.text <| Locale.i18n locale <| Locale.Contacts Locale.Author1]
    , Html.ul [] [
        Html.li [] [Html.text "+79506006622"]
      , Html.li [] [Html.text <| Locale.i18n locale <| Locale.Contacts <| Locale.Email "apuchenkin@gmail.com"]
      , Html.li [] [Html.text <| Locale.i18n locale <| Locale.Contacts <| Locale.Skype "apuchenkin"]
      ]
    ]
  , Html.div [Attr.class "details"] [
      Html.b [] [Html.text <| Locale.i18n locale <| Locale.Contacts Locale.Author2]
    , Html.ul [] [
        Html.li [] [Html.text <| Locale.i18n locale <| Locale.Contacts <| Locale.Email "kuzmicheva.tata@gmail.com"]
      , Html.li [] [Html.text <| Locale.i18n locale <| Locale.Contacts <| Locale.Skype "tata_kuzmicheva"]
      ]
    ]
  , Html.p [] [Html.text <| Locale.i18n locale <| Locale.Contacts <| Locale.Questions "info@photo.awesomestuff.in" ]
  ]

notFoundWidget : Router Route State -> Locale -> Html (Action State)
notFoundWidget = lazy2 <| \router locale -> Html.div [Attr.class "error-404"] [
    Html.h2 [] [Html.b [] [Html.text <| Locale.i18n locale <| Locale.Error <| Locale.DefaultError "404"], Html.text " - ", Html.text <| Locale.i18n locale <| Locale.Error Locale.NotFound]
  , Html.p [] [Html.text <| Locale.i18n locale <| Locale.Error Locale.Reasons]
  , Html.ol [] [
      Html.li [] [Html.text <| Locale.i18n locale <| Locale.Error Locale.Reason1]
    , Html.li [] [Html.text <| Locale.i18n locale <| Locale.Error Locale.Reason2]
    ]
  , Html.p [] [Html.text <| Locale.i18n locale <| Locale.Error Locale.Consequence]
  -- , Html.a [] [Html.text <| Locale.i18n locale "ERROR.URL.HOME" []]
  , homeLink router locale <| Locale.i18n locale <| Locale.Error Locale.HomeUrl
  ]

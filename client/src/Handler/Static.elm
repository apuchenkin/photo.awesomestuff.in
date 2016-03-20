module Handler.Static where

import Html exposing (Html, text, span)
import Html.Lazy exposing (lazy, lazy2)
import Html.Attributes as Attr
import Router.Types exposing (Router)

import App.Locale as Locale exposing (Locale)
import App.Routes as Routes exposing (Route)
import App.Model exposing (State)
import Handler.Widgets exposing (homeLink)

aboutWidget : Locale -> Html
aboutWidget = lazy <| \locale -> Html.div [Attr.class "about"] [
    Html.p [] [Html.text <| Locale.i18n locale "ABOUT.TEXT1" []]
  , Html.a [Attr.target "_blank", Attr.href <| Locale.i18n locale "FORM.BUGREPORT" []] [Html.text <| Locale.i18n locale "FORM.BUGREPORT" []]
  , Html.p [] [Html.text <| Locale.i18n locale "ABOUT.TEXT2" []]
  , Html.a [Attr.target "_blank", Attr.href <| Locale.i18n locale "FORM.FEEDBACK" []] [Html.text <| Locale.i18n locale "FORM.FEEDBACK" []]
  , Html.p [] [Html.text <| Locale.i18n locale "ABOUT.TEXT3" []]
  ]

contactsWidget : Locale -> Html
contactsWidget = lazy <| \locale -> Html.div [Attr.class "contacts"] [
    Html.h3 [] [Html.text <| Locale.i18n locale "CONTACTS.AUTHORS" []]
  , Html.div [Attr.class "details"] [
      Html.b [] [Html.text <| Locale.i18n locale "CONTACTS.AUTHOR1" []]
    , Html.ul [] [
        Html.li [] [Html.text "+79506006622"]
      , Html.li [] [Html.text <| Locale.i18n locale "CONTACTS.EMAIL" ["apuchenkin@gmail.com"]]
      , Html.li [] [Html.text <| Locale.i18n locale "CONTACTS.SKYPE" ["apuchenkin"]]
      ]
    ]
  , Html.div [Attr.class "details"] [
      Html.b [] [Html.text <| Locale.i18n locale "CONTACTS.AUTHOR2" []]
    , Html.ul [] [
        Html.li [] [Html.text <| Locale.i18n locale "CONTACTS.EMAIL" ["kuzmicheva.tata@gmail.com"]]
      , Html.li [] [Html.text <| Locale.i18n locale "CONTACTS.SKYPE" ["tata_kuzmicheva"]]
      ]
    ]
  , Html.p [] [Html.text <| Locale.i18n locale "CONTACTS.QUESTIONS" ["info@photo.awesomestuff.in"]]
  ]

notFoundWidget : Router Route State -> Locale -> Html
notFoundWidget = lazy2 <| \router locale -> Html.div [Attr.class "error-404"] [
    Html.h2 [] [Html.b [] [Html.text <| Locale.i18n locale "ERROR" ["404"]], Html.text " - ", Html.text <| Locale.i18n locale "ERROR.NOT_FOUND" []]
  , Html.p [] [Html.text <| Locale.i18n locale "ERROR.THIS_MIGHT_BE" []]
  , Html.ol [] [
      Html.li [] [Html.text <| Locale.i18n locale "ERROR.REASON1" []]
    , Html.li [] [Html.text <| Locale.i18n locale "ERROR.REASON2" []]
    ]
  , Html.p [] [Html.text <| Locale.i18n locale "ERROR.CONSEQUENCE" []]
  -- , Html.a [] [Html.text <| Locale.i18n locale "ERROR.URL.HOME" []]
  , homeLink router locale <| Locale.i18n locale "ERROR.URL.HOME" []
  ]

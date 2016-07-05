module App.Locale exposing (..)

import Date
import Dict exposing (Dict)

import App.Config exposing (config)

type Locale = Ru | En

fallbackLocale: Locale
fallbackLocale = En

locales : List Locale
locales = [Ru, En]

fromString : String -> Locale
fromString str = case str of
  "ru" -> Ru
  "en" -> En
  _ -> fallbackLocale

toString : Locale -> String
toString locale = case locale of
  Ru -> "ru"
  En -> "en"

type alias TranslationSet =
  { en : String
  , ru : String
  }

type Translation
  = Month String
  | Home
  | Galleries
  | Copy
  | Alfa
  | Title String
  | Subtitle
  | Date Date.Date
  | Author
  | Action TranslationAction
  | Form TranslationForm
  | About TraslationAbout
  | Contacts TranslationContacts
  | Error TranslationError
  | Meta TranslationMeta

type TranslationAction
  = Close
  | Prev
  | Next

translateAction : TranslationAction -> TranslationSet
translateAction action = case action of
    Close -> TranslationSet "Close" "Закрыть"
    Prev -> TranslationSet "Previous" "Предыдущая"
    Next -> TranslationSet "Next" "Следующая"

type TranslationForm
  = Bugreport
  | Feedback

translateForm : TranslationForm -> TranslationSet
translateForm translation = case translation of
    Bugreport -> TranslationSet "http://goo.gl/forms/PytHUvBm48" "http://goo.gl/forms/gjCXgnadYm"
    Feedback -> TranslationSet "http://goo.gl/forms/mD6GCLnzCT" "http://goo.gl/forms/jCZLgQfTx4"

type TraslationAbout
  = AboutTitle
  | About1
  | About2
  | About3

translateAbout : TraslationAbout -> TranslationSet
translateAbout translation = case translation of
    AboutTitle -> TranslationSet "About" "О сайте"
    About1 -> TranslationSet
      "This is alpha release of photo gallery. If you found any bugs, please, complete the following form (will open in a new page):"
      "Этот сайт находится в состоянии альфа-тестирования и может содержать ошибки. Если вы обнаружили ошибку, пожалуйста, укажите их на следующей форме (откроется в новом окне):"
    About2 -> TranslationSet
      "If you want to left feedback or have any improvement ideas, submit following:"
      "Если бы вы хотели поделиться впечатлением о веб-сайте, или у вас есть идеи как его можно было бы улучшить, заполните форму обратной связи:"
    About3 -> TranslationSet
      "Thanks for your attention!"
      "Спасибо за внимание!"

type TranslationContacts
  = ContactsTitle
  | Authors
  | Author1
  | Author2
  | Email String
  | Skype String
  | Questions String

translateContacts : TranslationContacts -> TranslationSet
translateContacts translation = case translation of
  ContactsTitle -> TranslationSet "Contacts" "Контакты"
  Authors -> TranslationSet "Authors:" "Авторы:"
  Author1 -> TranslationSet "Artem Puchenkin" "Артём Пученкин"
  Author2 -> TranslationSet "Tatiana Kuzmicheva" "Татьяна Кузмичева"
  Email email -> TranslationSet ("email: " ++ email) ("email: " ++ email)
  Skype skype -> TranslationSet ("skype: " ++ skype) ("skype: " ++ skype)
  Questions email -> TranslationSet ("Questions related to web-site: " ++ email) ("По вопросам о работе веб-сайта: " ++ email)

type TranslationError
  = DefaultError String
  | NotFound
  | Reasons
  | Reason1
  | Reason2
  | Consequence
  | BackUrl
  | HomeUrl

translateError : TranslationError -> TranslationSet
translateError translation = case translation of
  DefaultError error -> TranslationSet
    ("Error " ++ error)
    ("Ошибка " ++ error)
  NotFound -> TranslationSet
    "This page does not exists"
    "Cтраница не существует"
  Reasons -> TranslationSet
    "This might be because of:"
    "Это могло случиться по следующим причинам:"
  Reason1 -> TranslationSet
    "Page has not been created yet"
    "Страница ещё не была создана"
  Reason2 -> TranslationSet
    "Page has been deleted for some reason"
    "Страница была удалена"
  Consequence -> TranslationSet
    "If you want to help this page to be alive, please, send your ideas to: info 'at' photo.awesomestuff.in"
    "Если эта страница необходима, пожалуйста, напишите на: info 'at' photo.awesomestuff.in"
  BackUrl -> TranslationSet
    "Go back"
    "Назад"
  HomeUrl -> TranslationSet
    "Return home"
    "На главную"

type TranslationMeta
  = Description
  | PhotoDescription String String

translateMeta : TranslationMeta -> TranslationSet
translateMeta translation = case translation of
  Description -> TranslationSet
      "Travel Photography by Artem Puchenkin and Tatiana Kuzmicheva."
      "Туристическая фотография Пученина Артёма и Татьяны Кузмичевой - фотографии путешествий, интересных мест и событий."
  PhotoDescription author title -> TranslationSet
      ("Author: " ++ author ++ ", Title: " ++ title)
      ("Автор: " ++ author ++ ", Описание: " ++ title)

monthTranslations : Dict String TranslationSet
monthTranslations = Dict.fromList
  [ ( "Jan", TranslationSet "January" "Январь")
  , ( "Feb", TranslationSet "February" "Февраль")
  , ( "Mar", TranslationSet "March" "Март")
  , ( "Apr", TranslationSet "April" "Апрель")
  , ( "May", TranslationSet "May" "Май")
  , ( "Jun", TranslationSet "June" "Июнь")
  , ( "Jul", TranslationSet "July" "Июль" )
  , ( "Aug", TranslationSet "August" "Август" )
  , ( "Sep", TranslationSet "September" "Сентябрь" )
  , ( "Oct", TranslationSet "October" "Октябрь" )
  , ( "Nov", TranslationSet "November" "Ноябрь" )
  , ( "Dec", TranslationSet "December" "Декабрь" )
  ]

singleton : String -> TranslationSet
singleton translation = TranslationSet translation translation

translateDate : Locale -> Date.Date -> TranslationSet
translateDate locale date = singleton <| (i18n locale <| Month (Basics.toString <| Date.month date)) ++ ", "++ (Basics.toString <| Date.year date)

i18n : Locale -> Translation -> String
i18n locale trans =
  let
    translation = case trans of
      Month month -> case Dict.get month monthTranslations of
        Just set -> set
        _ -> Debug.crash <| "Month not translated: " ++ month
      Home -> TranslationSet "Home" "Главная"
      Galleries -> TranslationSet "Galleries" "Галереи"
      Copy -> TranslationSet "© 2015, Artem Puchenkin" "© 2015, Пученкин Артём"
      Alfa -> TranslationSet " alfa" " альфа"
      Title title -> singleton <| title ++ " - " ++ config.title
      Subtitle -> TranslationSet "Travel in photography" "Путешествия в фотографиях"
      Date date -> translateDate locale date
      Author -> TranslationSet "Author " "Автор "
      Action action -> translateAction action
      Form form -> translateForm form
      About translation -> translateAbout translation
      Contacts translation -> translateContacts translation
      Error translation -> translateError translation
      Meta translation -> translateMeta translation

  in case locale of
    En -> translation.en
    Ru -> translation.ru

module Handler.Locale where

import I18n exposing (withLanguage, createLookup, Language(Language))

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

month_en : List (String, String)
month_en =
  [ ( "Jan", "January" )
  , ( "Feb", "February")
  , ( "Mar", "March")
  , ( "Apr", "April")
  , ( "May", "May")
  , ( "Jun", "June")
  , ( "Jul", "July" )
  , ( "Aug", "August" )
  , ( "Sep", "September" )
  , ( "Oct", "October" )
  , ( "Nov", "November" )
  , ( "Dec", "December" )
  ]

month_ru : List (String, String)
month_ru =
  [ ( "Jan", "Январь" )
  , ( "Feb", "Февраль")
  , ( "Mar", "Март")
  , ( "Apr", "Апрель")
  , ( "May", "Май")
  , ( "Jun", "Июнь")
  , ( "Jul", "Июль" )
  , ( "Aug", "Август" )
  , ( "Sep", "Сентябрь" )
  , ( "Oct", "Октябрь" )
  , ( "Nov", "Ноябрь" )
  , ( "Dec", "Декабрь" )
  ]

lookup : Language -> String -> List String -> String
lookup =
    createLookup
        [ withLanguage
            (Language <| toString Ru) <|
            [ ( "Home", "Главная" )
            , ( "alpha", "альфа")
            , ( "About", "О сайте")
            , ( "Contacts", "Контакты")
            , ( "Galleries", "Галереи")
            , ( "Travel in photography", "Путешествия в фотографиях")
            , ( "© 2015, Artem Puchenkin", "© 2015, Пученкин Артём")
            , ( "I am {0} years old.", "J'ai {0} ans." )
            ]
            ++ month_ru
        , withLanguage
            (Language <| toString En)
            month_en
        ]

i18n : Locale -> String -> List String -> String
i18n locale = lookup (Language <| toString locale)

-- {
--   "META_DESCRIPTION": "Travel Photography by Artem Puchenkin and Tatiana Kuzmicheva.",
--   "META_DESCRIPTION_PHOTO": "Author: {{author}}, Title: {{title}}",
--   "GALLERIES": "",
--   "COPY": "2015, Artem Puchenkin",
--   "FOOTER": "{{home}}, alpha",
--   "RU": "Ru",
--   "EN": "En",
--   "ERROR": {
--     "404": "Error 404",
--     "NOT_EXISTS": "This page does not exists",
--     "THIS_MIGHT_BE": "This might be because of:",
--     "REASON1": "It have not been created yet",
--     "REASON2": "It have been deleted for some reason",
--     "CONSEQUENCE": "If you want to help this page to be alive, please, send your ideas to: info 'at' photo.awesomestuff.in",
--     "BACK": "Go back",
--     "HOME": "Return home"
--   },
--   "FORM": {
--     "BUGREPORT": "http://goo.gl/forms/PytHUvBm48",
--     "FEEDBACK": "http://goo.gl/forms/mD6GCLnzCT"
--   },
--   "TOOLS": {
--     "FULLSCREEN" : "Full screen",
--     "CLOSE": "Close"
--   },
--   "PHOTO": {
--     "BY": "by ",
--     "PAGES": "%curr% of %total%",
--     "PREV": "Previous (Left arrow key)",
--     "NEXT": "Next (Right arrow key)"
--   }
-- }

-- {
--   "META_DESCRIPTION": "Туристическая фотография Пученина Артёма и Татьяны Кузмичевой - фотографии путешествий, интересных мест и событий.",
--   "META_DESCRIPTION_PHOTO": "Автор: {{author}}, Описание: {{title}}",
--   "GALLERIES": "",
--   "COPY": "2015, Пученкин Артём",
--   "FOOTER": "{{home}}, alpha",
--   "RU": "Ru",
--   "EN": "En",
--   "ERROR": {
--     "404": "Ошибка 404",
--     "NOT_EXISTS": "Эта страница не существует",
--     "THIS_MIGHT_BE": "Это могло случиться по следующим причинам:",
--     "REASON1": "Страница ещё не была создана",
--     "REASON2": "Страница была удалена",
--     "CONSEQUENCE": "Если эта страница необходима, пожалйста, напишите на: info 'at' photo.awesomestuff.in",
--     "BACK": "Назад",
--     "HOME": "На главную"
--   },
--   "FORM": {
--     "BUGREPORT": "http://goo.gl/forms/gjCXgnadYm",
--     "FEEDBACK": "http://goo.gl/forms/jCZLgQfTx4"
--   },
--   "TOOLS": {
--     "FULLSCREEN" : "Увеличить",
--     "CLOSE": "Закрыть"
--   },
--   "PHOTO": {
--     "BY": "автор ",
--     "PAGES": "%curr% из %total%",
--     "PREV": "Предыдущая (Left arrow key)",
--     "NEXT": "Следующая (Right arrow key)"
--   }
-- }

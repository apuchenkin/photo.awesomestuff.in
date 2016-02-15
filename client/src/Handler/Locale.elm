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

-- english : Language
-- english =
--     Language "en-us"
--
-- french : Language
-- french =
--     Language "fr-fr"
--
-- german : Language
-- german =
--     Language "de-de"

lookup : Language -> String -> List String -> String
lookup =
    createLookup
        [ withLanguage
            (Language <| toString Ru)
            [ ( "HOME", "ДОМ" )
            , ( "I am {0} years old.", "J'ai {0} ans." )
            ]
        ]

i18n : Locale -> String -> List String -> String
i18n locale = lookup (Language <| toString locale)

-- {
--   "ALPHA": "alpha",
--   "DESCRIPTION": "Travel in photography",
--   "META_DESCRIPTION": "Travel Photography by Artem Puchenkin and Tatiana Kuzmicheva.",
--   "META_DESCRIPTION_PHOTO": "Author: {{author}}, Title: {{title}}",
--   "HOME": "Home",
--   "GALLERIES": "Galleries",
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
--   "STATIC": {
--     "ABOUT": "About",
--     "CONTACTS": "Contacts"
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
--   "ALPHA": "альфа",
--   "DESCRIPTION": "Путешествия в фотографиях",
--   "META_DESCRIPTION": "Туристическая фотография Пученина Артёма и Татьяны Кузмичевой - фотографии путешествий, интересных мест и событий.",
--   "META_DESCRIPTION_PHOTO": "Автор: {{author}}, Описание: {{title}}",
--   "HOME": "Главная",
--   "GALLERIES": "Галереи",
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
--   "STATIC": {
--     "ABOUT": "О сайте",
--     "CONTACTS": "Контакты"
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

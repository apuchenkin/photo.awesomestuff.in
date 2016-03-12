module App.Locale where

import I18n exposing (withLanguage, createLookup, Language(Language))
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
lookup = createLookup
  [ withLanguage
    (Language <| toString Ru) <|
    [ ( "Home", "Главная" )
    , ( "alpha", "альфа")
    , ( "about", "О сайте")
    , ( "contacts", "Контакты")
    , ( "Galleries", "Галереи")
    , ( "Travel in photography", "Путешествия в фотографиях")
    , ( "© 2015, Artem Puchenkin", "© 2015, Пученкин Артём")
    , ("TITLE", "{0} - " ++ config.title)
    , ("CLOSE", "Закрыть")
    , ("PREV", "Предыдущая")
    , ("NEXT", "Следующая")
    , ("FORM.BUGREPORT", "http://goo.gl/forms/gjCXgnadYm")
    , ("FORM.FEEDBACK", "http://goo.gl/forms/jCZLgQfTx4")
    , ("ABOUT.TEXT1", "Этот сайт находится в состоянии альфа-тестирования и может содержать ошибки. Если вы обнаружили ошибку, пожалуйста, укажите их на следующей форме (откроется в новом окне):")
    , ("ABOUT.TEXT2", "Если бы вы хотели поделиться впечатлением о веб-сайте, или у вас есть идеи как его можно было бы улучшить, заполните форму обратной связи:")
    , ("ABOUT.TEXT3", "Спасибо за внимание!")
    , ("CONTACTS.AUTHORS", "Авторы:")
    , ("CONTACTS.AUTHOR1", "Артём Пученкин")
    , ("CONTACTS.AUTHOR2", "Татьяна Кузмичева")
    , ("CONTACTS.EMAIL", "email: {0}")
    , ("CONTACTS.SKYPE", "skype: {0}")
    , ("CONTACTS.QUESTIONS", "По вопросам о работе веб-сайта: {0}")
    , ("ERROR", "Ошибка {0}")
    , ("ERROR.NOT_FOUND", "Cтраница не существует")
    , ("ERROR.THIS_MIGHT_BE", "Это могло случиться по следующим причинам:")
    , ("ERROR.REASON1", "Страница ещё не была создана")
    , ("ERROR.REASON2", "Страница была удалена")
    , ("ERROR.CONSEQUENCE", "Если эта страница необходима, пожалуйста, напишите на: info 'at' photo.awesomestuff.in")
    , ("ERROR.URL.BACK", "Назад")
    , ("ERROR.URL.HOME", "На главную")
    , ("META.DESCRIPTION", "Туристическая фотография Пученина Артёма и Татьяны Кузмичевой - фотографии путешествий, интересных мест и событий.")
    , ("META.DESCRIPTION.PHOTO", "Автор: {0}, Описание: {1}")
    ] ++ month_ru
  , withLanguage
    (Language <| toString En) <|
    [ ( "about", "About")
    , ( "contacts", "Contacts")
    , ("TITLE", "{0} - " ++ config.title)
    , ("CLOSE", "Close")
    , ("PREV", "Previous")
    , ("NEXT", "Next")
    , ("FORM.BUGREPORT", "http://goo.gl/forms/PytHUvBm48")
    , ("FORM.FEEDBACK", "http://goo.gl/forms/mD6GCLnzCT")
    , ("ABOUT.TEXT1", "This is alpha release of photo gallery. If you found any bugs, please, complete the following form (will open in a new page):")
    , ("ABOUT.TEXT2", "If you want to left feedback or have any improvement ideas, submit following:")
    , ("ABOUT.TEXT3", "Thanks for your attention!")
    , ("CONTACTS.AUTHORS", "Authors:")
    , ("CONTACTS.AUTHOR1", "Artem Puchenkin")
    , ("CONTACTS.AUTHOR2", "Tatiana Kuzmicheva")
    , ("CONTACTS.EMAIL", "email: {0}")
    , ("CONTACTS.SKYPE", "skype: {0}")
    , ("CONTACTS.QUESTIONS", "Questions related to web-site: {0}")
    , ("ERROR", "Error {0}")
    , ("ERROR.NOT_FOUND", "This page does not exists")
    , ("ERROR.THIS_MIGHT_BE", "This might be because of:")
    , ("ERROR.REASON1", "Page has not been created yet")
    , ("ERROR.REASON2", "Page has been deleted for some reason")
    , ("ERROR.CONSEQUENCE", "If you want to help this page to be alive, please, send your ideas to: info 'at' photo.awesomestuff.in")
    , ("ERROR.URL.BACK", "Go back")
    , ("ERROR.URL.HOME", "Return home")
    , ("META.DESCRIPTION", "Travel Photography by Artem Puchenkin and Tatiana Kuzmicheva.")
    , ("META.DESCRIPTION.PHOTO", "Author: {0}, Title: {1}")
    ] ++ month_en
  ]

i18n : Locale -> String -> List String -> String
i18n locale = lookup (Language <| toString locale)

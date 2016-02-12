module Handler.Locale where

type Locale = Ru | En

fallbackLocale: Locale
fallbackLocale = En

fromString : String -> Locale
fromString str = case str of
  "ru" -> Ru
  "en" -> En
  _ -> fallbackLocale

toString : Locale -> String
toString locale = case locale of
  Ru -> "ru"
  En -> "en"

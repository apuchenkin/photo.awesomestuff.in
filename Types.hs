{-# LANGUAGE TemplateHaskell #-}
module Types where

import ClassyPrelude.Yesod

data TranslationType = CategoryType | PhotoType
    deriving (Show, Read, Eq)
derivePersistField "TranslationType"

instance FromJSON TranslationType where
    parseJSON tt = case tt of
            "category"  -> pure CategoryType
            "photo"     -> pure PhotoType
            _           -> mempty

instance ToJSON TranslationType where
    toJSON CategoryType = "category"
    toJSON PhotoType    = "photo"

data Language = En | Ru
    deriving (Show, Read, Eq)
derivePersistField "Language"

pickLanguadge :: [Text] -> Language
pickLanguadge [] = En
pickLanguadge (x:_)
  | x == "ru" = Ru
  | otherwise = En

instance FromJSON Language where
    parseJSON lang = case lang of
            "en" -> pure En
            "ru" -> pure Ru
            _    -> mempty

instance ToJSON Language where
    toJSON En = "en"
    toJSON Ru = "ru"

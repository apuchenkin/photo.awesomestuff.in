{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Types where

import ClassyPrelude.Yesod

data TranslationType = CategoryType | PhotoType | PageType
    deriving (Show, Read, Eq)
derivePersistField "TranslationType"

instance FromJSON TranslationType where
    parseJSON tt = case tt of
            "category"  -> pure CategoryType
            "photo"     -> pure PhotoType
            "page"      -> pure PageType
            _           -> mempty

instance ToJSON TranslationType where
    toJSON CategoryType = "category"
    toJSON PhotoType    = "photo"
    toJSON PageType     = "page"

data Language = En | Ru
    deriving (Show, Read, Eq)
derivePersistField "Language"

pickLanguadge :: [Text] -> Language
pickLanguadge [] = En
pickLanguadge (x:_)
  | x == "ru"     = Ru
  | x == "ru-RU"  = Ru
  | otherwise     = En

instance FromJSON Language where
    parseJSON lang = case lang of
            "en" -> pure En
            "ru" -> pure Ru
            _    -> mempty

instance ToJSON Language where
    toJSON En = "en"
    toJSON Ru = "ru"

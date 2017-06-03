{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import
import qualified Data.HashMap.Strict     as H (union, fromList)
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Value
getHomeR = returnJson ()

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

getTranslations :: (ToJSON (Entity record), ToBackendKey SqlBackend record) => Entity record -> TranslationType -> Maybe [String] -> Handler Value
getTranslations entity typ fields = do
  langs         <- languages
  translations  <- runDB
      $ E.select
      $ E.from $ \translation -> do
        E.where_ $ translation ^. TranslationLanguage E.==. E.val (pickLanguadge langs)
        E.where_ $ translation ^. TranslationRefType  E.==. E.val typ
        E.where_ $ translation ^. TranslationRefId    E.==. E.val (E.fromSqlKey $ E.entityKey entity)

        case fields of
          Nothing -> return ()
          Just fields' -> E.where_ $ translation ^. TranslationField `E.in_` E.valList fields'

        return translation

  let values = flip map translations $ \ (Entity _ t) -> (pack $ translationField t, toJSON $ translationValue t)
  return $ Object $ H.fromList values

getLangs :: (ToJSON (Entity record), ToBackendKey SqlBackend record) => Entity record -> TranslationType -> [String] -> Handler Value
getLangs entity typ fields = do
  langs  <- runDB
      $ E.select
      $ E.from $ \translation -> do
        E.where_ $ translation ^. TranslationRefType  E.==. E.val typ
        E.where_ $ translation ^. TranslationRefId    E.==. E.val (E.fromSqlKey $ E.entityKey entity)
        E.where_ $ translation ^. TranslationField   `E.in_` E.valList fields
        E.groupBy $ translation ^. TranslationLanguage

        return (translation ^. TranslationLanguage, E.countRows)

  return $ parseResult langs
    where
      parseResult :: [(E.Value Language, E.Value Int)] -> Value
      parseResult langs = toJSON $ map (E.unValue . fst) $ filter (\(_,c) -> E.unValue c == length fields) langs

appendTranslations :: (ToJSON (Entity record), ToBackendKey SqlBackend record) => [Entity record] -> TranslationType -> Maybe [String] -> Handler Value
appendTranslations list typ fields = do
  langs         <- languages
  translations  <- runDB
      $ E.select
      $ E.from $ \translation -> do
        E.where_ $ translation ^. TranslationLanguage E.==. E.val (pickLanguadge langs)
        E.where_ $ translation ^. TranslationRefType  E.==. E.val typ
        E.where_ $ translation ^. TranslationRefId   `E.in_` E.valList (toKeys list)

        case fields of
          Nothing -> return ()
          Just fields' -> E.where_ $ translation ^. TranslationField `E.in_` E.valList fields'


        return translation

  returnJson $ map (parseResult translations) list
  where
  toKeys = map (\(Entity key _) -> E.fromSqlKey key)
  parseResult :: (ToJSON (Entity record), ToBackendKey SqlBackend record) => [Entity Translation] -> Entity record -> Value
  parseResult translations (Entity key entity) = Object $ H.union e tl
    where
      filtered = flip filter translations $ \ (Entity _ t) -> translationRefId t == E.fromSqlKey key
      translationValues = flip map filtered $ \ (Entity _ t) -> (pack $ translationField t, toJSON $ translationValue t)
      (Object e) = toJSON $ Entity key entity
      tl = H.fromList translationValues

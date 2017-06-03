{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Page where

import Import
import Handler.Common                    (getTranslations, getLangs)
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Database.Persist.Sql    (fromSqlKey)
import qualified Data.HashMap.Strict     as H (insert, union, filterWithKey)
import Database.Esqueleto.Internal.Sql   (veryUnsafeCoerceSqlExprValue)

-- import Data.HashMap.Strict ((!))
-- import Data.Aeson.Types (parseEither)
-- import Control.Failure (Failure (failure))

coerce :: E.SqlExpr (E.Value Int64) -> E.SqlExpr (E.Value (Key Page))
coerce = veryUnsafeCoerceSqlExprValue

getPagesR :: Handler Value
getPagesR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- month
  addHeader "Vary" "Accept-Language, Authorization"
  maid <- maybeAuthId
  langs <- languages
  pages <- runDB
      $ E.select
      $ E.from $ \(page `E.InnerJoin` translation) -> do
        E.on $ page ^. PageId E.==. coerce ( translation ^. TranslationRefId )
        E.where_ $ translation ^. TranslationLanguage E.==. E.val (pickLanguadge langs)
        E.where_ $ translation ^. TranslationRefType  E.==. E.val PageType
        E.where_ $ translation ^. TranslationField    E.==. E.val "title"

        case maid of
          Nothing -> E.where_ (page ^. PageHidden E.==. E.val False)
          Just _  -> return ()

        return (page, translation ^. TranslationValue)

  returnJson $ map (updatePages maid) pages
  where
    expose = ["id", "alias", "parent"]
    updatePages :: Maybe UserId -> (Entity Page, E.Value Text) -> Value
    updatePages maid (page, title) = Object hmap'
      where
          (Object c) = toJSON page
          hmap = case maid of
            Nothing -> H.filterWithKey (\k _ -> elem k expose) c
            Just _ -> c
          hmap' = H.insert "title" (toJSON $ E.unValue title) hmap

-- postPagesR :: Language -> Handler Value
-- postPagesR lang = do
--   object <- getRequestBody :: Handler Value
--   let page = parseEither parseJSON object :: Either String Page
--   either (failure internalError) persistPage page
--
--   where
--     persistPage :: Page -> Handler Value
--     persistPage page = do
--       epid <- runDB $ insertBy page
--       let pid = either entityKey id epid
--       let titleTranslation = Translation En PageType (fromSqlKey pid) "title" (object ! "title")
--       insertUnique titleTranslation
--       let contentTranslation = Translation En PageType (fromSqlKey pid) "content" (object ! "content")
--       insertUnique contentTranslation
--
--       return pid

getPageR :: PageId -> Handler Value
getPageR pageId = do
    cacheSeconds $ 60 * 60 * 24 * 30 -- month
    addHeader "Vary" "Accept-Language"
    maid    <- maybeAuthId
    mpage   <- runDB $ get pageId
    page    <- maybe notFound return mpage
    _       <- when (pageHidden page && isNothing maid) notFound

    let entity = Entity pageId page
    (Object translations) <- getTranslations entity PageType Nothing
    langs <- getLangs entity PageType ["title", "content"]

    let expose = ["id", "alias", "parent"]
        (Object r) = toJSON entity
        result = H.filterWithKey (\k _ -> elem k expose) r
        result' = H.union result translations
        result'' = H.insert "langs" langs result'

    return $ Object result''

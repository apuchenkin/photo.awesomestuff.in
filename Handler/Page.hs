module Handler.Page where

import Import
import Handler.Common                    (appendTranslations)
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Database.Persist.Sql    (fromSqlKey)
import qualified Data.HashMap.Strict     as H (union, filterWithKey)
import qualified Handler.Photo           as Photo
import Data.HashMap.Strict ((!))
import Data.Aeson.Types (parseEither)
import Control.Failure (Failure (failure))

getPagesR :: Handler Value
getPagesR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- month
  addHeader "Vary" "Accept-Language, Authorization"
  maid    <- maybeAuthId
  pages   <- runDB
      $ E.select
      $ E.from $ \page -> do

        case maid of
          Nothing -> E.where_ (page ^. PageHidden E.==. E.val False)
          Just _  -> return ()

        return page

  returnJson pages

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
    langs   <- languages
    maid    <- maybeAuthId
    mpage   <- runDB $ get pageId
    page    <- maybe notFound return mpage
    _       <- when (pageHidden page && isNothing maid) notFound
    mt      <- runDB $ getBy $ UniqueTranslation
      (pickLanguadge langs)
      PageType
      (fromSqlKey pageId)
      "title"

    mc      <- runDB $ getBy $ UniqueTranslation
      (pickLanguadge langs)
      PageType
      (fromSqlKey pageId)
      "content"

    let expose = ["id", "alias", "category_id", "caption", "content", "children"]
        (Object r) = toJSON $ Entity pageId page
        (Object e) = object [
            "title"     .= fmap (translationValue . entityVal) mt,
            "content"   .= fmap (translationValue . entityVal) mc
          ]

    return $ Object $ H.union e $ H.filterWithKey (\k _ -> elem k expose) r

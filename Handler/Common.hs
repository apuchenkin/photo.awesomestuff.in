-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import
import qualified Data.HashMap.Strict     as H (union, fromList)
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

appendTranslations :: (ToJSON (Entity record), ToBackendKey SqlBackend record) => [Entity record] -> TranslationType -> Handler Value
appendTranslations list typ = do
  langs         <- languages
  translations  <- runDB
      $ E.select
      $ E.from $ \translation -> do
        E.where_ $ translation ^. TranslationLanguage E.==. E.val (pickLanguadge langs)
        E.where_ $ translation ^. TranslationType     E.==. E.val typ

        return translation

  returnJson $ map (parseResult translations) list
  where
  parseResult :: (ToJSON (Entity record), ToBackendKey SqlBackend record) => [Entity Translation] -> Entity record -> Value
  parseResult translations (Entity key entity) = Object $ H.union e tl
    where
      filtered = flip filter translations $ \ (Entity _ t) -> translationRefId t == E.fromSqlKey key
      translationValues = flip map filtered $ \ (Entity _ t) -> (pack $ translationField t, toJSON $ translationValue t)
      (Object e) = toJSON $ Entity key entity
      tl = H.fromList translationValues
module Handler.Category where

import Import
import Database.Persist.Sql              (Single, unSingle, rawSql)
import qualified Data.HashMap.Strict     as H (union)

getCategoryR :: Handler Value
getCategoryR = do
  cacheSeconds 604800
  addHeader "Vary" "Accept-Language"
  result <- runDB $ rawSql
    "SELECT ??, translation.value \
      FROM category INNER JOIN translation \
      ON  translation.ref_id = category.id \
      AND translation.type      = ? \
      AND translation.language  = ? \
      AND translation.field     = ?"
    [toPersistValue CategoryType, toPersistValue En, PersistText "title"]

  returnJson $ map parseResult result
  where
    parseResult :: (Entity Category, Single Text) -> Value
    parseResult (category, title) = Object $ H.union e c
      where
        (Object c) = toJSON category
        (Object e) = object [
            "title" .= unSingle title
          ]

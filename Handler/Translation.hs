module Handler.Translation where

import Import

getTranslationR :: Handler Value
getTranslationR = do
    translations <- runDB $ selectList [] [] :: Handler [Entity Translation]
    cacheSeconds 86400
    returnJson translations
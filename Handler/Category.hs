module Handler.Category where

import Import

getCategoryR :: Handler Value
getCategoryR = do
    categories <- runDB $ selectList [] [] :: Handler [Entity Category]
    returnJson $ categories

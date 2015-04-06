{-# LANGUAGE FlexibleInstances #-}

module Handler.Home where

import Import
import System.Directory (getDirectoryContents)
--import qualified GHC.IO  as IO (FilePath)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Value
getHomeR = do
     returnJson ()

getInstallR :: Handler Value
getInstallR = do
     let path = "static/gallery/src/"
     srcs <- liftIO $ getDirectoryContents path
     ids <- sequence $ map (createPhoto path) srcs
     returnJson ids
     where
     createPhoto :: String -> String -> Handler (Key Photo)
     createPhoto srcPath = (\ src -> do
        let thumbPath = "static/gallery/thumb/"
        let thumb = Just $ thumbPath ++ src
        let photo = Photo src (srcPath ++ src) thumb 0 Nothing
        photoId <- runDB $ insert photo
        return photoId)
{-# LANGUAGE FlexibleInstances #-}

module Handler.Home where

import Import
import System.Directory (getDirectoryContents)
import Codec.Picture (readImage, Image (..))
import Codec.Picture.Types (dynamicMap)
import Data.Aeson (decode, encode, Object)
import Data.Aeson.Types (parseMaybe)

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
     createPhoto :: String -> String -> Handler (Maybe (Key Photo))
     createPhoto srcPath = \ name -> do
        let src = srcPath ++ name
        mImage <- liftIO $ readImage src
        case mImage of
            Left _  -> return Nothing
            Right image -> do
                let thumbPath = "static/gallery/thumb/"
                let thumb = Just $ thumbPath ++ name
                let width  = dynamicMap imageWidth  image
                let height = dynamicMap imageHeight image
                let photo = Photo name src thumb width height "" 0
                photoId <- runDB $ insert photo
                return $ Just photoId

getInstallExifR :: Handler Value
getInstallExifR = do
    exifFile <- liftIO $ readFile "exif.json"
    let maybeExif = decode exifFile :: Maybe [Object]
    case maybeExif of
        Nothing -> invalidArgs ["category"]
        Just exif -> do
            ids <- sequence $ map createPhoto exif
            returnJson ids
            where
                createPhoto :: Object -> Handler (Key Photo)
                createPhoto obj = do
                    let maybePhoto = flip parseMaybe obj $ \o -> do
                         name   <- o  .: "File:FileName"
                         src    <- o  .: "SourceFile"
                         width  <- o  .: "File:ImageWidth"
                         height <- o  .: "File:ImageHeight"
                         let thumb  = Just ("static/gallery/thumb/" ++ name)
                         let exifData = toStrict $ decodeUtf8 $ encode obj
                         return $ Photo name src thumb width height exifData 0
                    case maybePhoto of
                        Nothing -> invalidArgs ["parse error"]
                        Just photo -> do
                            photoId <- runDB $ insert photo
                            return photoId
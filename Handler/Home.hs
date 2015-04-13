{-# LANGUAGE FlexibleInstances #-}

module Handler.Home where

import Import
import System.Directory (getDirectoryContents)
import Codec.Picture (readImage, Image (..))
import Codec.Picture.Types (dynamicMap)
import Data.Aeson (decode, encode, Object)
import Data.Aeson.Types (parseMaybe, Parser)
import Database.Persist.Sql (fromSqlKey)
import qualified Data.Text as T (replace)

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
                createPhoto :: Object -> Handler (Maybe (Key Photo))
                createPhoto obj = do
                    maybeCategories <- fromMaybe (return []) $ flip parseMaybe obj $ \o -> do
                         categories <- o  .: "IPTC:Keywords"  :: Parser [Text]
                         return $ sequence $ flip map categories $ \title -> do
                            let normalizedTitle = unpack $ T.replace " " "-" (toLower title)
                            mcid <- runDB $ insertUnique $ Category normalizedTitle Nothing
                            mcid <- case mcid of
                                Nothing -> do
                                   mc <- runDB $ getBy $ UniqueName normalizedTitle
                                   case mc of
                                        Nothing -> return Nothing
                                        Just c  -> return $ Just $ entityKey c
                                Just cid -> return $ Just cid
                            case mcid of
                                Nothing     -> return Nothing
                                Just cid    -> do
                                    _ <- runDB $ insertUnique $ Translation En CategoryType (fromSqlKey cid) "title" (unpack $ title)
                                    return $ Just cid

                    maybePhoto <- fromMaybe (return Nothing) $ flip parseMaybe obj $ \o -> do
                         name       <- o  .: "File:FileName" :: Parser String
                         src        <- o  .: "SourceFile"
                         width      <- o  .: "File:ImageWidth"
                         height     <- o  .: "File:ImageHeight"
                         dir        <- o  .: "File:Directory"
                         author     <- o  .: "EXIF:Artist"
                         caption    <- o  .: "EXIF:Software"
                         let thumb  = Just (dir ++ "/thumb/" ++ name)
                         let exifData = toStrict $ decodeUtf8 $ encode obj
                         let photo = Photo name src thumb width height exifData 0
                         let insertPhoto = do
                              pid <- runDB $ insert photo
                              _ <- runDB $ insertUnique $ Translation En PhotoType (fromSqlKey pid) "author" author
                              _ <- runDB $ insertUnique $ Translation En PhotoType (fromSqlKey pid) "caption" caption
                              return $ Just pid
                         return insertPhoto

                    case maybePhoto of
                        Just pid -> do
                            _ <- sequence $ flip map (catMaybes maybeCategories) $ \cid -> runDB $ insertUnique $ PhotoCategory cid pid
                            return $ Just pid
                        Nothing ->
                            return Nothing




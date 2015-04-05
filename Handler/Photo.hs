module Handler.Photo where

import Import
import System.Directory
import qualified Vision.Image as I
--import Codec.Picture

getPhotoR :: Handler Value
getPhotoR = error "Not yet implemented: getPhotoR"

getPhotosR :: Handler Value
getPhotosR = do
    photos <- runDB $ selectList [] [] :: Handler [Entity Photo]
    srcs <- liftIO $ getDirectoryContents "static/gallery/src"
    let images = map readImage srcs
    flip map images (\image -> case image of
        Left err           -> do
            putStrLn "Unable to load the image:"
            print err
        Right rgb -> do
            let -- Resizes the RGB image to 250x250 pixels.
                resized =  I.resize  I.Bilinear (I.ix2 250 250) rgb :: RGB

            -- Saves the resized image. Automatically infers the output format.
            mErr <- save Autodetect output resized
            case mErr of
                Nothing  ->
                    putStrLn "Success."
                Just err -> do
                    putStrLn "Unable to save the image:"
                    print err
        )

    returnJson $ images

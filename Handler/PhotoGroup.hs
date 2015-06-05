module Handler.PhotoGroup where

import Import
import qualified Network.Wai            as W
import           Data.Aeson              (eitherDecodeStrict)
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Data.List               as L (head)

postPhotoGroupsR :: Handler Value
postPhotoGroupsR = do
  request <- waiRequest
  body    <- liftIO $ W.requestBody request
  let epid = eitherDecodeStrict body :: Either String [PhotoId]
  case epid of
    Left err    -> invalidArgs [pack err]
    Right pids  -> do
      mphotos <- runDB $ mapM get pids
      let photos = catMaybes mphotos
      let isInGroup = all (\Photo {..} -> isNothing photoGroup) photos
      case isInGroup of
        False -> invalidArgs ["One of the selected photos is already in group"]
        True  -> do
          result  <- runDB
            $ E.select
            $ E.from $ \(photo) -> do
              E.orderBy [E.desc (photo ^. PhotoGroup)]
              E.limit 1
              return (photo ^. PhotoGroup)

          let groupId = succ $ fromMaybe 0 (E.unValue $ L.head result) :: Int
          _ <- runDB $ mapM (updateGroup groupId) pids
          returnJson groupId
          where
            updateGroup gid pid = update pid [PhotoGroup =. Just gid]

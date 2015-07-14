module Handler.PhotoGroup where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Data.List               as L (head)

postPhotoGroupsR :: Handler Value
postPhotoGroupsR = do
  pids    <- getRequestBody :: Handler [PhotoId]
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

linkPhotoGroupR :: Int -> Handler Value
linkPhotoGroupR gid = do
  pids <- getRequestBody :: Handler [PhotoId]
  pcid <- runDB $ mapM (`update` [PhotoGroup =. Just gid]) pids
  returnJson pcid

unlinkPhotoGroupR :: Int -> Handler Value
unlinkPhotoGroupR _ = do
  pids <- getRequestBody :: Handler [PhotoId]
  pcid <- runDB $ mapM (`update` [PhotoGroup =. Nothing]) pids
  returnJson pcid

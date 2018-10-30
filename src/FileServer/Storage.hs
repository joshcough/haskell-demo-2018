
module FileServer.Storage (
    FilesDb(..)
  ) where

import           Control.Monad               (forM)
import           Control.Monad.Except        (MonadIO)
import           Database.Persist.Postgresql (insert)
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (Text)
import           Database.Esqueleto

import           Config                      (AppT', runDb)
import           Auth.DatabaseModels         (DbUserId)
import           FileServer.DatabaseModels   (DbFolder(..), DbFolderId, DbFile(..), DbFileId, EntityField(..))
import           FileServer.Models           (Folder(..))

class Monad m => FilesDb m where
    insertFile :: DbUserId -> DbFolderId -> Text -> m (Entity DbFile)
    insertFolder :: DbUserId -> DbFolderId -> Text -> m (Key DbFolder)
    getRootFolder :: DbUserId -> m (Maybe (Folder (Entity DbFile)))
    getFile :: DbFileId -> m (Maybe (Entity DbFile))
    -- | Purposely doesn't fetch the children, because we don't want to use recursion right now.
    getFolder :: DbFolderId -> m (Maybe (Folder (Entity DbFile)))
    getFolderRecursive :: DbFolderId -> m (Maybe (Folder (Entity DbFile)))
    deleteFile :: DbFileId -> m ()
    deleteFolder :: DbFolderId -> m ()
    moveFile :: DbFileId -> Folder a -> m ()
    moveFolder :: Folder a -> Folder a -> m ()

instance MonadIO m => FilesDb (AppT' e m) where
    insertFile teamId folderId fileName = runDb $ insertFile teamId folderId fileName
    insertFolder teamId parentId folderName = runDb $ insertFolder teamId parentId folderName
    getFolder = runDb . getFolder
    getRootFolder = runDb . getRootFolder
    getFile = runDb . getFile
    deleteFolder = runDb . deleteFolder
    deleteFile = runDb . deleteFile
    getFolderRecursive = runDb . getFolderRecursive
    moveFolder folder moveTo = runDb $ moveFolder folder moveTo
    moveFile fileId moveTo = runDb $ moveFile fileId moveTo

instance MonadIO m => FilesDb (SqlPersistT m) where
    insertFile teamId folderId fileName = do
        let f = DbFile teamId folderId fileName
        k <- insert f
        return $ Entity k f

    insertFolder teamId parentId folderName = insert $ DbFolder teamId parentId folderName

    getFolder fid = do
        folder  <- get fid
        files   <- getFilesInFolder fid
        folders <- getFoldersInFolder fid
        return $ entityToFolder files folders . Entity fid <$> folder

    getRootFolder tid = getRootFolderId tid >>= maybe (return Nothing) getFolder

    getFile fid = fmap (Entity fid) <$> get fid

    deleteFolder = deleteCascade

    deleteFile = deleteKey

    -- | This is so inefficient... can we write a real db query to do this?
    getFolderRecursive fid = do
        mFolder <- get fid
        forM mFolder $ \folder -> do
            files    <- getFilesInFolder fid
            folders  <- fmap (fmap entityKey) $ select $ from $ \f -> do
                            where_ $ (f ^. DbFolderParentId) ==. val fid
                            return f
            folders' <- forM folders getFolderRecursive
            return $ entityToFolder files (catMaybes folders') (Entity fid folder)

    -- TODO: must insert checks that folder isn't itself or a child of itself! otherwise code will infinite loop
    moveFolder folder moveTo = update $ \f -> do
        set f [ DbFolderParentId =. val (toSqlKey $ folderId moveTo) ]
        where_ (f ^. DbFolderId ==. val (toSqlKey $ folderId folder) )

    moveFile fileId moveTo = update $ \f -> do
        set f [ DbFileFolderId =. val (toSqlKey $ folderId moveTo) ]
        where_ (f ^. DbFileId ==. val fileId )

-- |
getRootFolderId :: MonadIO m => DbUserId -> SqlPersistT m (Maybe DbFolderId)
getRootFolderId tid = fmap f $ select $ from $ \folder -> do
    where_ (
      (folder ^. DbFolderUserId)   ==. val tid
      &&.
      (folder ^. DbFolderParentId) ==. val (toSqlKey 0)
     )
    return folder
    where f [folder] = Just $ entityKey folder
          f _        = Nothing -- TODO: ... not sure what to do here. would represent a bad error.

-- |
getFilesInFolder :: MonadIO m => DbFolderId -> SqlPersistT m [Entity DbFile]
getFilesInFolder fid = select $ from $ \f -> do
    where_ $ (f ^. DbFileFolderId) ==. val fid
    return f

-- | Purposely doesn't fetch the children, because we don't want to use recursion right now.
getFoldersInFolder :: MonadIO m => DbFolderId -> SqlPersistT m [Folder (Entity DbFile)]
getFoldersInFolder fid = fmap (fmap $ entityToFolder [] []) $ select $ from $ \f -> do
    where_ $ (f ^. DbFolderParentId) ==. val fid
    return f

-- |
entityToFolder :: [file] -> [Folder file] -> Entity DbFolder -> Folder file
entityToFolder files folders (Entity k DbFolder {..}) = Folder {
    folderId       = fromSqlKey k
  , folderUserId   = fromSqlKey dbFolderUserId
  , folderParentId = pid
  , folderName     = dbFolderName
  , folderFiles    = files
  , folderFolders  = folders
  }
  where
    pid = case fromSqlKey dbFolderParentId of
        0 -> Nothing -- TODO: this magic is for the "root" folder, but it might need work
        i -> Just i


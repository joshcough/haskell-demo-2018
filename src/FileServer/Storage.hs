
module FileServer.Storage (
    FilesDb(..)
  ) where

import           Control.Monad.Except        (MonadIO)
import           Database.Persist.Postgresql (insert)
import           Data.Text                   (Text)
import           Database.Esqueleto

import           Config                      (AppT', runDb)
import           Auth.DatabaseModels         (DbUserId)
import           FileServer.DatabaseModels   (DbFile(..), DbFileId)

class Monad m => FilesDb m where
    insertFile :: DbUserId -> Text -> m (Entity DbFile)
    getFile :: DbFileId -> m (Maybe (Entity DbFile))
    deleteFile :: DbFileId -> m ()

instance MonadIO m => FilesDb (AppT' e m) where
    insertFile teamId fileName = runDb $ insertFile teamId fileName
    getFile = runDb . getFile
    deleteFile = runDb . deleteFile

instance MonadIO m => FilesDb (SqlPersistT m) where
    insertFile teamId fileName = do
        let f = DbFile teamId fileName
        k <- insert f
        return $ Entity k f

    getFile fid = fmap (Entity fid) <$> get fid

    deleteFile = deleteKey

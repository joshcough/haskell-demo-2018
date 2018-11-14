{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FileServer.Storage (
    DbFile(..)
  , DbFileId
  , FilesDb(..)
  ) where

import           Control.Monad.Except             (MonadIO)
import           Database.Esqueleto
import           Database.Persist.Postgresql      (insert)
import           Database.Persist.Postgresql.JSON ()
import           Database.Persist.TH              (mkDeleteCascade, mkPersist, persistLowerCase, share, sqlSettings)
import           Data.Text                        (Text)

import           Config                           (AppT', runDb)
import           Auth.DatabaseModels              (DbUserId)

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings] [persistLowerCase|
DbFile json sql=files
    userId             DbUserId
    originalFileName   Text sql=original_file_name
    deriving Show Eq
|]

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

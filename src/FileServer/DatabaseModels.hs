{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FileServer.DatabaseModels where

import           Database.Persist.Postgresql.JSON ()
import           Database.Persist.TH              (mkDeleteCascade, mkPersist, persistLowerCase, share, sqlSettings)
import           Data.Text                        (Text)

import           Auth.DatabaseModels              (DbUserId)

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings] [persistLowerCase|
DbFolder json sql=folders
    userId              DbUserId
    parentId            DbFolderId
    name                Text
    DbFolderUniquePath  parentId name
    deriving Show Eq

DbFile json sql=files
    userId             DbUserId
    folderId           DbFolderId
    originalFileName   Text sql=original_file_name
    deriving Show Eq
|]

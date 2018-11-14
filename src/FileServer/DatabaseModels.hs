{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FileServer.DatabaseModels where

import           Database.Persist.Postgresql.JSON ()
import           Database.Persist.TH              (mkDeleteCascade, mkPersist, persistLowerCase, share, sqlSettings)
import           Data.Text                        (Text)

import           Auth.DatabaseModels              (DbUserId)

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings] [persistLowerCase|
DbFile json sql=files
    userId             DbUserId
    originalFileName   Text sql=original_file_name
    deriving Show Eq
|]

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module FileServer.Models (
    File(..)
  , Folder(..)
  , MoveRequest(..)
  , listFilesRecursive
  ) where

import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Int                       (Int64)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)

data Folder file = Folder {
    folderId       :: Int64
  , folderUserId   :: Int64
  , folderParentId :: Maybe Int64
  , folderName     :: Text
  , folderFiles    :: [file]
  , folderFolders  :: [Folder file]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable)

data File = File {
    fileId       :: Int64
  , fileUserId   :: Int64
  , fileFolderId :: Int64
  , fileName     :: Text
  , fileS3File   :: Text
  , fileS3Path   :: Text
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data MoveRequest = MoveRequest {
    moveTo :: Int64
  , moveFiles :: [Int64]
  , moveFolders :: [Int64]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

listFilesRecursive :: Folder file -> [file]
listFilesRecursive Folder{..} = folderFiles ++ do
    childFolder <- folderFolders
    listFilesRecursive childFolder

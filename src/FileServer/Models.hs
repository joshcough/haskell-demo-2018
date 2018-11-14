{-# LANGUAGE DeriveAnyClass #-}

module FileServer.Models (
    File(..)
  ) where

import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Int                       (Int64)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)

data File = File {
    fileId :: Int64
  , fileUserId :: Int64
  , fileName :: Text
  , fileS3File :: Text
  , fileS3Path :: Text
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)


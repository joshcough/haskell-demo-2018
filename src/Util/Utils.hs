
module Util.Utils (
    average
  , getRecursiveContents
  , strength
  , (^?)
  , tShow
) where

import           Control.Lens                   ((^?))
import           Control.Monad                  (forM)
import           Control.Monad.Except           (MonadIO, liftIO)
import           Data.List                      (genericLength)
import           Data.Text                      (Text, pack)
import           System.Directory               (doesDirectoryExist, getDirectoryContents)
import           System.FilePath                ((</>))

average :: Real a => [a] -> Double
average xs = realToFrac (sum xs) / genericLength xs

-- From Real World Haskell, p. 214
getRecursiveContents :: MonadIO m => FilePath -> m [FilePath]
getRecursiveContents topPath = liftIO $ do
  names <- getDirectoryContents topPath
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

-- | For some theory about this function, see https://bartoszmilewski.com/2017/02/06/applicative-functors/
strength :: Functor f => (a, f b) -> f (a, b)
strength (a, fb) = (a,) <$> fb

-- |
tShow :: Show a => a -> Text
tShow = pack . show
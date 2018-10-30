
module KeyGen (
    genDevKey , readDevKey
  , genProdKey, readProdKey
  , genTestKey, readTestKey
  ) where

import           Crypto.JOSE.JWK                 (JWK)
import           Data.Aeson                      (eitherDecode)
import           Data.Aeson.Encode.Pretty        (encodePretty)
import           Servant.Auth.Server
import qualified Data.ByteString.Lazy      as BS (readFile, writeFile)

---

prodKeyFile :: FilePath
prodKeyFile = "config/prod/jwt.key"

genProdKey :: IO ()
genProdKey = writeKeyFile prodKeyFile

readProdKey :: IO JWK
readProdKey = readKeyFile prodKeyFile

---

devKeyFile :: FilePath
devKeyFile = "config/dev/jwt.key"

genDevKey :: IO ()
genDevKey = writeKeyFile devKeyFile

readDevKey :: IO JWK
readDevKey = readKeyFile devKeyFile

---

testKeyFile :: FilePath
testKeyFile = "config/test/jwt.key"

genTestKey :: IO ()
genTestKey = writeKeyFile testKeyFile

readTestKey :: IO JWK
readTestKey = readKeyFile testKeyFile

---

readKeyFile :: FilePath -> IO JWK
readKeyFile f = BS.readFile f >>= either error pure . eitherDecode

writeKeyFile :: FilePath -> IO ()
writeKeyFile f = generateKey >>= BS.writeFile f . encodePretty

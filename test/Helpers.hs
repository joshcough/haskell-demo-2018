
module Helpers (
  -- Common users used throughout testing
    testTeam1Name, testTeam1, createTeam1Obj, testTeam1V, createTeam1V,
    adminUsername, adminEmail, adminPassword, adminSettings, adminUser,
    testUser1Username, testUser1Email, testUser1Password, testUser1Settings,
    testUser1, testUser1V, testUser1Login, testUser1LoginV, createTestUser1V,
    testUser2Username, testUser2Email, testUser2Password, testUser2Settings,
    testUser2, testUser2V, testUser2Login, testUser2LoginV, createTestUser2V,
    testUser1Token, testUser2Token, adminToken
  -- Wai Spec Helpers
  , postJson, putJson, postJson', putJson', get', delete', withAdminAuth, withNonAdminAuth, withUserAuth
  -- Setup and teardown helpers
  , runAppToIO, runServer, setupTeardown, setupTeardownDb,  testApp, cleanDbAndStartApp
) where

import           Control.Concurrent          (forkIO)
import           Control.Exception           (ErrorCall(..), throwIO)
import           Control.Monad.Except        (MonadIO, liftIO)
import           Data.Aeson                  (ToJSON(..), Value(..), object)
import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Internal    as BS (c2w)
import qualified Data.ByteString.Lazy        as LBS
import           Data.List                   (intercalate)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text, pack)
import           Data.UUID                   (UUID, fromText)
import           Database.Persist.Sql        (rawExecute)
import           Network.HTTP.Types.Header   (HeaderName)
import           Network.HTTP.Types.Method   (Method, methodDelete, methodGet, methodPost, methodPut)
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Test            (SResponse)
import           Servant.Auth.Server         (makeJWT)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON         (json)

import           Api                         (app)
import           Auth.Models                 (CreateTeam(..), CreateUser(..), Login(..), Team(..), User(..))
import           Config                      (App, Config (..), runAppTInTest, runDb)
import           Init                        (acquireConfig)

---
--- Setup and teardown helpers
---

runAppToIO :: Config -> App a -> IO a
runAppToIO config app' = do
    result <- runAppTInTest app' config
    either (throwIO . fmap (const (ErrorCall "error"))) return result

runServer :: IO ()
runServer = do
    config <- getTestConfig
    app'   <- testApp
    _ <- forkIO $ run (_configPort config) app'
    return ()

setupTeardown :: (Config -> IO ()) -> IO ()
setupTeardown runTestsWith = do
    config <- getTestConfig
    setupTeardownDb config
    runTestsWith config

-- https://stackoverflow.com/questions/5342440/reset-auto-increment-counter-in-postgres
setupTeardownDb :: Config -> IO ()
setupTeardownDb config = runAppToIO config . runDb $ truncateTables
    where
    tables = ["teams", "users", "scenes", "scene_items"]
    truncateTables = rawExecute (pack $ "TRUNCATE TABLE " ++ intercalate ", " tables ++ " RESTART IDENTITY CASCADE") []

testApp :: IO Application
testApp = app <$> getTestConfig

cleanDbAndStartApp :: IO Application
cleanDbAndStartApp = do
    config <- getTestConfig
    setupTeardownDb config
    return $ app config

getTestConfig :: IO Config
getTestConfig = acquireConfig

---
--- Wai Spec Helpers
---

postJson :: ToJSON a => BS.ByteString -> a -> WaiSession SResponse
postJson = putOrPostJson methodPost

putJson :: ToJSON a => BS.ByteString -> a -> WaiSession SResponse
putJson = putOrPostJson methodPut

putOrPostJson :: ToJSON a => Method -> BS.ByteString -> a -> WaiSession SResponse
putOrPostJson method path = request method path [("content-type", "application/json")] . encodePretty

postJson' :: (Show u, ToJSON a) => BS.ByteString -> u -> a -> WaiSession SResponse
postJson' = putOrPostJson' methodPost

putJson' :: (Show u, ToJSON a) => BS.ByteString -> u -> a -> WaiSession SResponse
putJson' = putOrPostJson' methodPut

putOrPostJson' :: (Show u, ToJSON a) => Method -> BS.ByteString -> u -> a -> WaiSession SResponse
putOrPostJson' method path u = request' method path [("content-type", "application/json")] u . encodePretty

-- | Request with authorization header
request' :: Show u => Method -> BS.ByteString -> [(HeaderName, BS.ByteString)] -> u -> LBS.ByteString -> WaiSession SResponse
request' method path headers u = request method path (("Authorization", auth) : headers)
    where auth = BS.pack $ BS.c2w <$> "Bearer " ++ unquotedShow u
          unquotedShow = dropRight 1 . drop 1 . show
          dropRight n = reverse . drop n . reverse

get' :: Show a => BS.ByteString -> a -> WaiSession SResponse
get' path a = request' methodGet path [] a ""

delete' :: Show a => BS.ByteString -> a -> WaiSession SResponse
delete' path a = request' methodDelete path [] a ""

withAdminAuth :: MonadIO m => (LBS.ByteString -> m a) -> m a
withAdminAuth = withAuth adminUser

withNonAdminAuth :: MonadIO m => (LBS.ByteString -> m a) -> m a
withNonAdminAuth = withAuth testUser1

withUserAuth :: (LBS.ByteString -> WaiSession a) -> WaiSession a
withUserAuth f = withAdminAuth $ \u -> do
    postJson' "/teams" u createTeam1V     `shouldRespondWith` [json|1|]
    postJson' "/users" u createTestUser1V `shouldRespondWith` [json|1|]
    withAuth testUser1 f

withAuth :: MonadIO m => User -> (LBS.ByteString -> m a) -> m a
withAuth u f = do
    jwtCfg <- liftIO $ _configJWT <$> getTestConfig
    etoken <- liftIO $ makeJWT u jwtCfg Nothing
    case etoken of
         Left e -> error $ "Error generating token:t" ++ show e
         Right v -> f v

---
--- Common test data used throughout testing
---

testTeam1Name :: Text
testTeam1Name = "USA"

testUser1Username, testUser1Email, testUser1Password :: Text
testUser1Username = "daut"
testUser1Email    = "daut@daut.com"
testUser1Password = "password1"

testUser2Username, testUser2Email, testUser2Password :: Text
testUser2Username = "viper"
testUser2Email    = "viper@daut.com"
testUser2Password = "password2"

adminUsername, adminEmail, adminPassword :: Text
adminUsername = "josh"
adminEmail    = "josh@troll.com"
adminPassword = "password2"

testUser1Token, testUser2Token, adminToken :: UUID
testUser1Token = fromJust $ fromText "550e8400-e29b-41d4-a716-446655440000"
testUser2Token = fromJust $ fromText "550e8400-e29b-41d4-a716-446655440000"
adminToken     = fromJust $ fromText "550e8400-e29b-41d4-a716-446655440000"

testUser1Settings, testUser2Settings, adminSettings :: Value
testUser1Settings = object []
testUser2Settings = object []
adminSettings     = object []

testTeam1 :: Team
testTeam1 = Team 1 "USA" testUser1Settings

testUser1 :: User
testUser1 = User 1 (teamId testTeam1) testUser1Username testUser1Email testUser1Settings False testUser1Token

testUser2 :: User
testUser2 = User 2 (teamId testTeam1) testUser2Username testUser2Email testUser2Settings False testUser2Token

adminUser :: User
adminUser = User 1 (teamId testTeam1) adminUsername adminEmail adminSettings True adminToken

testUser1Login :: Login
testUser1Login = Login testUser1Email testUser1Password

testUser2Login :: Login
testUser2Login = Login testUser2Email testUser2Password

createTeam1Obj :: CreateTeam
createTeam1Obj = CreateTeam (teamName testTeam1) testUser1Settings

createTestUser1Obj :: CreateUser
createTestUser1Obj = CreateUser (userName testUser1) (teamId testTeam1) (userEmail testUser1) testUser1Password testUser1Settings

createTestUser2Obj :: CreateUser
createTestUser2Obj = CreateUser (userName testUser2) (teamId testTeam1) (userEmail testUser2) testUser2Password testUser2Settings

testTeam1V, createTeam1V, testUser1V, testUser1LoginV, createTestUser1V, testUser2V, testUser2LoginV, createTestUser2V :: Value
testTeam1V       = toJSON testTeam1
createTeam1V     = toJSON createTeam1Obj
testUser1V       = toJSON testUser1
testUser1LoginV  = toJSON testUser1Login
createTestUser1V = toJSON createTestUser1Obj
testUser2V       = toJSON testUser2
testUser2LoginV  = toJSON testUser2Login
createTestUser2V = toJSON createTestUser2Obj
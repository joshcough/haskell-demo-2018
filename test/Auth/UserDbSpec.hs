
module Auth.UserDbSpec (spec, withUser) where

import           Test.Hspec

import           Auth.UserAPI
import qualified Auth.Storage.UserStorage as Db
import           Auth.Models
import           Config                      (AppT, Config, runDb)
import           Database.Persist.Postgresql (toSqlKey)
import           Helpers

spec :: Spec
spec = before_ runServer $ around setupTeardown $
    describe "User" $ do
        it "getUserById fetches User by name" $ \config -> do
            (User _ name email) <- withUser config return
            name     `shouldBe` testUser1Username
            email    `shouldBe` testUser1Email

        it "deleteUser deletes user" $ \config -> do
            mUser <- withUser config $ \u -> do
                deleteUser adminUser (userIdKey u)
                runDb $ Db.getUserById (userIdKey u)
            mUser `shouldBe` Nothing

        it "updateUser updates user" $ \config -> do
            let troll      = "artoftroll"
            let trollEmail =  "artoftroll@yourmom.com"
            u' <- withUser config $ \u@(User uid _ _) -> do
                updateUser  u (userIdKey u) $ User uid troll trollEmail
                getUserById u (userIdKey u)
            userName  u' `shouldBe` troll
            userEmail u' `shouldBe` trollEmail

withUser :: Config -> (User -> AppT IO a) -> IO a
withUser config f = runAppToIO config $ do
    _    <- createUser adminUser $ CreateUser adminUsername     adminEmail     adminPassword
    uid  <- createUser adminUser $ CreateUser testUser1Username testUser1Email testUser1Password
    user <- getUserById adminUser uid
    f user

userIdKey = toSqlKey . userId
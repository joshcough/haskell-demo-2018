
module Auth.UserDbSpec (spec, withUser) where

import           Data.Aeson                  (object)
import           Test.Hspec

import           Auth.TeamAPI                (createTeam)
import           Auth.UserAPI
import qualified Auth.Storage.UserStorage as Db
import           Auth.Models
import           Config                      (AppT, Config, runDb)
import           Database.Persist.Postgresql (fromSqlKey, toSqlKey)
import           Helpers

spec :: Spec
spec = before_ runServer $ around setupTeardown $
    describe "User" $ do
        it "getUserById fetches User by name" $ \config -> do
            (User _ _ name email settings _ _) <- withUser config return
            name     `shouldBe` testUser1Username
            email    `shouldBe` testUser1Email
            settings `shouldBe` testUser1Settings

        it "deleteUser deletes user" $ \config -> do
            mUser <- withUser config $ \u -> do
                deleteUser adminUser (userIdKey u)
                runDb $ Db.getUserById (userIdKey u)
            mUser `shouldBe` Nothing

        it "updateUser updates user" $ \config -> do
            let troll      = "artoftroll"
            let trollEmail =  "artoftroll@yourmom.com"
            let trollSett  = object []
            u' <- withUser config $ \u@(User uid tid _ _ _ _ _) -> do
                updateUser  u (userIdKey u) $ User uid tid troll trollEmail trollSett False testUser1Token -- False for not admin
                getUserById u (userIdKey u)
            userName  u' `shouldBe` troll
            userEmail u' `shouldBe` trollEmail

withUser :: Config -> (User -> AppT IO a) -> IO a
withUser config f = runAppToIO config $ do
    tid  <- createTeam adminUser createTeam1Obj
    _    <- createUser adminUser $ CreateUser adminUsername     (fromSqlKey tid) adminEmail     adminPassword     (object [])
    uid  <- createUser adminUser $ CreateUser testUser1Username (fromSqlKey tid) testUser1Email testUser1Password (object [])
    user <- getUserById adminUser uid
    f user

userIdKey = toSqlKey . userId
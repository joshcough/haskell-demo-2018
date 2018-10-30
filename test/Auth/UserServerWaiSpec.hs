
module Auth.UserServerWaiSpec (spec) where

import           Data.Aeson                  (Value(..))
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           Helpers

spec :: Spec
spec = with cleanDbAndStartApp $ do
    describe "GET /users/1" $
        it "responds with 404" $ withAdminAuth $ \u ->
            get' "/users/1" u `shouldRespondWith` 404

    describe "POST /users followed by GET /users/1" $
        it "responds with a user id" $ withUserAuth $ \u ->
            get' "/users/1" u `shouldRespondWith` 200
                -- TODO: fix
                -- [json|{"userIsAdmin":false,"userSettings":{},"userName":"daut","userTeamId":1,"userId":1,"userEmail":"daut@daut.com"}|]

    describe "POST /users followed by PUT /users/1" $
        it "responds with a user id" $ withUserAuth $ \u ->
            putJson'  "/users/1" u testUser1V  `shouldRespondWith` 200

    describe "POST followed by DELETE followed by GET" $
        it "responds with 404" $ withAdminAuth $ \a -> withUserAuth $ \u -> do
            delete'   "/users/1" a             `shouldRespondWith` 200
            get'      "/users/1" u             `shouldRespondWith` 404

    describe "Update user settings json" $
        it "updates" $ withUserAuth $ \u -> do
            postJson' "/users/1/settings/x" u (Number 6) `shouldRespondWith` 200
            get' "/users/1" u `shouldRespondWith` 200
                -- TODO: fix... can we use something like the matchHeaders below?
                -- [json| {"userIsAdmin":false,"userSettings":{"x":6},"userName":"daut","userTeamId":1,"userId":1,"userEmail":"daut@daut.com"}|]
            get' "/users/1/settings/x" u `shouldRespondWith` [json|6|]

-- TODO: fix. this is broken because we are generating the token when we create the user, and this causes
-- TODO: the token to change. grassSad.
--     let jwtHeader :: MatchHeader
--         jwtHeader = "Set-Cookie" <:> "JWT-Cookie=eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsidXNlcklzQWRtaW4iOmZhbHNlLCJ1c2VyU2V0dGluZ3MiOnt9LCJ1c2VyTmFtZSI6ImRhdXQiLCJ1c2VyVGVhbUlkIjoxLCJ1c2VySWQiOjEsInVzZXJUb2tlbiI6IjhlMjYwY2RjLTdlYWYtNGJmOS05YWY1LWQwOGU5YzY1NjUyMSIsInVzZXJFbWFpbCI6ImRhdXRAZGF1dC5jb20ifX0.Pjyg9ajAIdKZHv_Ga85z7R9nCKBX21oD3y7Cn1NafDQTJ6sQVUXXJhpNDnUtjRGqTCM1--fLdkkSUROpMkIc6w; Path=/; HttpOnly; Secure"

    describe "login" $
        it "sets set-cookie header" . withUserAuth $ \_ ->
            postJson  "/login" testUser1Login `shouldRespondWith` 200 -- {matchHeaders = [jwtHeader]}

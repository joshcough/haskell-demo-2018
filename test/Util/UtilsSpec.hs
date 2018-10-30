
module Util.UtilsSpec (spec) where

import           Data.Aeson                     (Value(..), object)
import           Helpers                        (testTeam1V)
import           Test.Hspec
import           Util.Utils                     (setJsonPath')

a :: Value
a = object [("a", Number 7)]

xy :: Value -> Value
xy body = object [("x", object [("y", body)])]

xyz :: Value -> Value
xyz body = xy $ object [("z", body)]

xyab :: Value -> Value -> Value
xyab aBody bBody = object [("x", object [("y",  object [("a", aBody), ("b", bBody)])])]

empty :: Value
empty = object []

spec :: Spec
spec =
    describe "Util Test suite" $ do
        it "sets root values" $
            setJsonPath' "x" a empty `shouldBe` object [("x", a)]

        it "sets nested values even when they dont exist" $
            setJsonPath' "x.y.z" testTeam1V empty `shouldBe` xyz testTeam1V

        it "sets nested values when they do exist" $
            setJsonPath' "x.y.z" (Number 5) (xyz a) `shouldBe` xyz  (Number 5)

        it "preserves siblings" $
            setJsonPath' "x.y.a" (Number 5) (xyab (Number 6) (Number 7)) `shouldBe` xyab (Number 5) (Number 7)


-- import           Test.QuickCheck
-- it "does properties" $ property $ \x -> x + 1 > (x :: Int)


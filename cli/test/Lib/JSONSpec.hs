module Lib.JSONSpec
    ( spec
    )
where

import Lib.JSON
    ( CanonicalJSONError (CanonicalJSONError)
    , runCanonicalJSON
    , runIdentityCanonicalJSON
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.JSON.Canonical
    ( FromJSON (..)
    , Int54
    , JSValue (..)
    , ToJSON (..)
    )

spec :: Spec
spec = do
    describe "CanonicalJSON monad" $ do
        it "can be run on Identity" $ do
            let result = runIdentityCanonicalJSON $ toJSON JSNull
            result `shouldBe` Right JSNull
        it "can be run on IO" $ do
            result <- runCanonicalJSON $ toJSON (JSString "test")
            result `shouldBe` Right (JSString "test")
        it "catches errors" $ do
            result :: Either CanonicalJSONError Int54 <-
                runCanonicalJSON $ fromJSON (JSObject mempty)
            result
                `shouldBe` Left
                    (CanonicalJSONError "int" $ Just "object")

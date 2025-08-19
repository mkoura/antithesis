module AdversarySpec where

import Adversary
import Data.Aeson (decode, encode)
import Test.Hspec (Spec, it, shouldBe, shouldReturn)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, forAll)

spec :: Spec
spec = do
    it "Returns startup message" $ do
        adversary [] `shouldReturn` Startup{arguments = []}

    it "Display Message as String"
        $ toString (Startup{arguments = ["Foo"]})
        `shouldBe` "{\"arguments\":[\"Foo\"]}"

    prop "Roundtrip messages to/from JSON" prop_roundTrip

prop_roundTrip :: Property
prop_roundTrip = forAll genMessage $ \msg ->
    let encoded = encode msg
        decoded = decode encoded
    in  decoded == Just msg

genMessage :: Gen Message
genMessage = Startup <$> arbitrary

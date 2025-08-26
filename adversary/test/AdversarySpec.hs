module AdversarySpec where

import Adversary
import Data.Aeson (decode, encode)
import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, forAll)
import Adversary.ChainSync (Limit(..))

spec :: Spec
spec = do

    it "Display Message as String"
        $ toString (Startup{arguments = ["Foo"]})
        `shouldBe` "{\"arguments\":[\"Foo\"]}"

    it "Reads Limit"
        $ read "20" `shouldBe` Limit 20

    prop "Roundtrip messages to/from JSON" prop_roundTrip

prop_roundTrip :: Property
prop_roundTrip = forAll genMessage $ \msg ->
    let encoded = encode msg
        decoded = decode encoded
    in  decoded == Just msg

genMessage :: Gen Message
genMessage = Startup <$> arbitrary

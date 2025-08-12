module User.Agent.TypesSpec
    ( spec
    , genWhiteListKey
    , genRepository
    , genAscii
    )
where

import Core.Types.Basic
    ( Platform (Platform)
    , Repository (..)
    )
import Data.Char (isAscii)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Testable (..)
    , forAll
    , listOf
    , suchThat
    )
import Text.JSON.Canonical
    ( FromJSON (..)
    , ToJSON (..)
    )
import User.Agent.Types (WhiteListKey (..))

genAscii :: Gen [Char]
genAscii = listOf $ arbitrary `suchThat` isAscii

genRepository :: Gen Repository
genRepository = do
    owner <- genAscii
    Repository owner <$> genAscii

roundTrip
    :: (ToJSON Maybe a, FromJSON Maybe a, Show a, Eq a) => a -> IO ()
roundTrip value = do
    let decoded = toJSON value >>= fromJSON
    decoded `shouldBe` Just value

genWhiteListKey :: Gen WhiteListKey
genWhiteListKey = do
    platform <- arbitrary `suchThat` all isAscii
    WhiteListKey (Platform platform) <$> genRepository

spec :: Spec
spec = do
    describe "WhiteListKey" $ do
        it "roundtrips on the JSON instance"
            $ property
            $ forAll genWhiteListKey
            $ \key ->
                roundTrip key

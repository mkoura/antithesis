module Lib.SSH.KeySpec
    ( spec
    )
where

import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteString qualified as B
import Lib.SSH.Private
    ( KeyAPI (KeyAPI, publicKey, sign)
    , decodePrivateSSHFile
    )
import Test.Hspec (Spec, beforeAll, describe, it)
import Test.QuickCheck (Testable (property))

readKey :: IO KeyAPI
readKey = do
    signingMap <- decodePrivateSSHFile "pw" "test/fixtures/test_ed25519"
    Just api <- pure $ signingMap "test_user"
    pure api

spec :: Spec
spec = do
    describe "SSH Key" $ beforeAll readKey $ do
        it "should sign and verify a message" $ \KeyAPI{sign, publicKey} -> do
            property $ \msgb -> do
                let msg = B.pack msgb
                let signed = sign msg
                Ed25519.verify publicKey msg signed

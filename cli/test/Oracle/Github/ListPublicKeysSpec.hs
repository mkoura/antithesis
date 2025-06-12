{-# LANGUAGE OverloadedStrings #-}

module Oracle.Github.ListPublicKeysSpec
    ( spec
    )
where

import Oracle.Github.ListPublicKeysIO
    ( GithubAccessToken (..)
    )
import Oracle.Github.ListPublicKeys
    ( PublicKeyValidation (..)
    , inspectPublicKeyTemplate
    )
import Test.Hspec
    ( Spec
    , it
    , shouldReturn
    )
import Types (Username (..), PublicKeyHash (..) )

import qualified Data.ByteString.Char8 as BC

dummyAccessToken :: IO GithubAccessToken
dummyAccessToken =
    pure $ GithubAccessToken $ BC.pack
    "github_pat_XXXXXXXXXXXXXXX_YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY"

spec :: Spec
spec = do
    it "user needs to have public key exposed" $ do
        let emptyPubKeyOfUser _ _ = pure []
            user = Username "user1"
            pubkey = PublicKeyHash ""
        inspectPublicKeyTemplate user pubkey dummyAccessToken emptyPubKeyOfUser
        `shouldReturn` NoPublicKeyFound

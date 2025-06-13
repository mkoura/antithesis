{-# LANGUAGE OverloadedStrings #-}

module Oracle.Github.OracleValidationSpec
    ( spec
    )
where

import Oracle.Github.CommonIO
    ( GithubAccessToken (..)
    )
import Oracle.Github.GetRepoRole
    ( RepoRoleValidation (..)
    , inspectRepoRoleForUserTemplate
    )
import Oracle.Github.GetRepoRoleIO
    ( ResponseRepoRole (..)
    )
import Oracle.Github.ListPublicKeysIO
    ( ResponsePublicKey (..)
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
import Types (Username (..), PublicKeyHash (..), Repository (..), Role (..) )

import qualified Data.ByteString.Char8 as BC

mockedAccessToken :: IO GithubAccessToken
mockedAccessToken =
    pure $ GithubAccessToken $ BC.pack
    "github_pat_XXXXXXXXXXXXXXX_YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY"

spec :: Spec
spec = do
    it "user needs to have public key(s) exposed" $ do
        let emptyPubKeyOfUser _ _ = pure []
            user = Username "user1"
            pubkey = PublicKeyHash ""
        inspectPublicKeyTemplate user pubkey mockedAccessToken emptyPubKeyOfUser
        `shouldReturn` NoPublicKeyFound

    it "user needs to have ssh-ed25519 public key exposed" $ do
        let respKey = ResponsePublicKey 1 "ssh-rsa AAAAAAAA"
            nonEd25519PubKeyOfUser _ _ = pure [respKey]
            user = Username "user1"
            pubkey = PublicKeyHash ""
        inspectPublicKeyTemplate user pubkey mockedAccessToken nonEd25519PubKeyOfUser
        `shouldReturn` NoEd25519KeyFound

    it "user needs to the expected ssh-ed25519 public key exposed" $ do
        let respKey = ResponsePublicKey 1 "ssh-ed25519 AAAAAAAA"
            noExpectedEd25519PubKeyOfUser _ _ = pure [respKey]
            user = Username "user1"
            pubkey = PublicKeyHash "XAAAAAAY"
        inspectPublicKeyTemplate user pubkey mockedAccessToken noExpectedEd25519PubKeyOfUser
        `shouldReturn` NoEd25519KeyMatch

    it "user needs gets the expected ssh-ed25519 public key exposed 1" $ do
        let respKey = ResponsePublicKey 1 "ssh-ed25519 XAAAAAAY"
            okExpectedEd25519PubKeyOfUser _ _ = pure [respKey]
            user = Username "user1"
            pubkey = PublicKeyHash "XAAAAAAY"
        inspectPublicKeyTemplate user pubkey mockedAccessToken okExpectedEd25519PubKeyOfUser
        `shouldReturn` PublicKeyValidated

    it "user needs gets the expected ssh-ed25519 public key exposed 1" $ do
        let respKey1 = ResponsePublicKey 1 "ssh-ed25519 XAAAAAAY"
            respKey2 = ResponsePublicKey 2 "ssh-ed25519 AAAAAAAA"
            respKey3 = ResponsePublicKey 3 "ssh-rsa XXXXXXXXXXXXXXXXXXXXXXx"
            okExpectedEd25519PubKeyOfUser _ _ = pure [respKey1, respKey2, respKey3]
            user = Username "user1"
            pubkey = PublicKeyHash "XAAAAAAY"
        inspectPublicKeyTemplate user pubkey mockedAccessToken okExpectedEd25519PubKeyOfUser
        `shouldReturn` PublicKeyValidated

    it "user does not have any right to the repo" $ do
        let noPermissions _ _ _ = pure $ ResponseRepoRole "none"
            user = Username "user1"
            repo = Repository "org" "repo"
            roleExp = Role ""
        inspectRepoRoleForUserTemplate user repo roleExp mockedAccessToken noPermissions
        `shouldReturn` NoRoleInRepo

    it "user tries to set non-existent role in the repo" $ do
        let permissionReps _ _ _ = pure $ ResponseRepoRole "write"
            user = Username "user1"
            repo = Repository "org" "repo"
            roleExp = Role "rite"
        inspectRepoRoleForUserTemplate user repo roleExp mockedAccessToken permissionReps
        `shouldReturn` NonexistantRolePicked

    it "user tries to validate the role that is different than what repo has set" $ do
        let permissionReps _ _ _ = pure $ ResponseRepoRole "read"
            user = Username "user1"
            repo = Repository "org" "repo"
            roleExp = Role "write"
        inspectRepoRoleForUserTemplate user repo roleExp mockedAccessToken permissionReps
        `shouldReturn` WrongRolePicked

    it "user's role in a given api is validated" $ do
        let permissionReps _ _ _ = pure $ ResponseRepoRole "write"
            user = Username "user1"
            repo = Repository "org" "repo"
            roleExp = Role "write"
        inspectRepoRoleForUserTemplate user repo roleExp mockedAccessToken permissionReps
        `shouldReturn` RepoRoleValidated

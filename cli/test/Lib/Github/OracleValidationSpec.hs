module Lib.Github.OracleValidationSpec
    ( spec
    )
where

import Core.Types.Basic
    ( PublicKeyHash (..)
    , Repository (..)
    , Username (..)
    )
import Data.Text qualified as T
import Lib.GitHub
    ( GetCodeOwnersFileFailure (..)
    , githubGetCodeOwnersFile
    )
import Test.Hspec
    ( Spec
    , it
    , shouldReturn
    )
import Validation.RegisterRole
    ( RepositoryRoleFailure (..)
    , inspectRepoRoleForUserTemplate
    )
import Validation.RegisterUser
    ( PublicKeyFailure (..)
    , inspectPublicKeyTemplate
    )

spec :: Spec
spec = do
    it "user needs to have public key(s) exposed"
        $ do
            let emptyPubKeyOfUser _ = pure $ Right []
                user = Username "user1"
                pubkey = PublicKeyHash ""
            inspectPublicKeyTemplate
                user
                pubkey
                emptyPubKeyOfUser
        `shouldReturn` Just NoPublicKeyFound

    it "user needs to have ssh-ed25519 public key exposed"
        $ do
            let respKey = "ssh-rsa AAAAAAAA"
                nonEd25519PubKeyOfUser _ = pure $ Right [respKey]
                user = Username "user1"
                pubkey = PublicKeyHash ""
            inspectPublicKeyTemplate
                user
                pubkey
                nonEd25519PubKeyOfUser
        `shouldReturn` Just NoEd25519KeyFound

    it "user needs to the expected ssh-ed25519 public key exposed"
        $ do
            let respKey = "ssh-ed25519 AAAAAAAA"
                noExpectedEd25519PubKeyOfUser _ = pure $ Right [respKey]
                user = Username "user1"
                pubkey = PublicKeyHash "XAAAAAAY"
            inspectPublicKeyTemplate
                user
                pubkey
                noExpectedEd25519PubKeyOfUser
        `shouldReturn` Just NoEd25519KeyMatch

    it "user needs gets the expected ssh-ed25519 public key exposed 1"
        $ do
            let respKey = "ssh-ed25519 XAAAAAAY"
                okExpectedEd25519PubKeyOfUser _ = pure $ Right [respKey]
                user = Username "user1"
                pubkey = PublicKeyHash "XAAAAAAY"
            inspectPublicKeyTemplate
                user
                pubkey
                okExpectedEd25519PubKeyOfUser
        `shouldReturn` Nothing

    it "user needs gets the expected ssh-ed25519 public key exposed 1"
        $ do
            let respKey1 = "ssh-ed25519 XAAAAAAY"
                respKey2 = "ssh-ed25519 AAAAAAAA"
                respKey3 = "ssh-rsa XXXXXXXXXXXXXXXXXXXXXXx"
                okExpectedEd25519PubKeyOfUser _ = pure $ Right [respKey1, respKey2, respKey3]
                user = Username "user1"
                pubkey = PublicKeyHash "XAAAAAAY"
            inspectPublicKeyTemplate
                user
                pubkey
                okExpectedEd25519PubKeyOfUser
        `shouldReturn` Nothing

    it "should download CODEOWNERS file from repo with main" $ do
        githubGetCodeOwnersFile
            (Repository "cardano-foundation" "hal-fixture-sin")
            `shouldReturn` Right "antithesis: @notunrandom @cfhal\n"

    it "should download CODEOWNERS file from repo with master" $ do
        githubGetCodeOwnersFile
            (Repository "cardano-foundation" "hal-fixture-cos")
            `shouldReturn` Right "* @notunrandom\n"

    it "should download CODEOWNERS file from repo with trunk" $ do
        githubGetCodeOwnersFile
            (Repository "cardano-foundation" "hal-fixture-tan")
            `shouldReturn` Right "* @notunrandom\n"

    it "should throw if missing CODEOWNERS file" $ do
        githubGetCodeOwnersFile
            (Repository "cardano-foundation" "hal-fixture-sec")
            `shouldReturn` Left GetCodeOwnersFileDirectoryNotFound

    it "CODEOWNERS does not have role entry" $ do
        let noRoleEntry _ =
                pure
                    $ Right
                    $ T.unlines
                        [ "# Haskell components"
                        , "core        /     @user"
                        , "command-line/     @user"
                        ]
            user = Username "user1"
            repo = Repository "org" "repo"
        inspectRepoRoleForUserTemplate user repo noRoleEntry
            `shouldReturn` Just NoRoleEntryInCodeowners

    it "CODEOWNERS does not have users assigned" $ do
        let noRoleEntry _ =
                pure
                    $ Right
                    $ T.unlines
                        [ "# Haskell components"
                        , "core        /     @user"
                        , "command-line/     @user"
                        , ""
                        , "antithesis:"
                        ]
            user = Username "user1"
            repo = Repository "org" "repo"
        inspectRepoRoleForUserTemplate user repo noRoleEntry
            `shouldReturn` Just NoUsersAssignedToRoleInCodeowners

    it "CODEOWNERS does have other users assigned" $ do
        let noRoleEntry _ =
                pure
                    $ Right
                    $ T.unlines
                        [ "# Haskell components"
                        , "core        /     @user"
                        , "command-line/     @user"
                        , ""
                        , "antithesis: @user1 @user3"
                        ]
            user = Username "user2"
            repo = Repository "org" "repo"
        inspectRepoRoleForUserTemplate user repo noRoleEntry
            `shouldReturn` Just NoUserInCodeowners

    it "CODEOWNERS does have user assigned" $ do
        let noRoleEntry _ =
                pure
                    $ Right
                    $ T.unlines
                        [ "# Haskell components"
                        , "core        /     @user"
                        , "command-line/     @user"
                        , ""
                        , "antithesis: @user1 @user2 @user3"
                        ]
            user = Username "user2"
            repo = Repository "org" "repo"
        inspectRepoRoleForUserTemplate user repo noRoleEntry
            `shouldReturn` Nothing

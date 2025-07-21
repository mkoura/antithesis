module Lib.Github.OracleValidationSpec
    ( spec
    )
where

import Core.Types.Basic
    ( PublicKeyHash (..)
    , Repository (..)
    , Username (..)
    )
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Lib.Github.GetRepoRole
    ( RepoRoleValidation (..)
    , inspectRepoRoleForUserTemplate
    )
import Lib.Github.GetRepoRoleIO
    ( ResponseCodeownersFile (..)
    , downloadCodeownersFile
    )
import Lib.Github.ListPublicKeys
    ( PublicKeyValidation (..)
    , inspectPublicKeyTemplate
    )
import Test.Hspec
    ( Spec
    , anyException
    , it
    , shouldReturn
    , shouldThrow
    )

spec :: Spec
spec = do
    it "user needs to have public key(s) exposed"
        $ do
            let emptyPubKeyOfUser _ = pure []
                user = Username "user1"
                pubkey = PublicKeyHash ""
            inspectPublicKeyTemplate
                user
                pubkey
                emptyPubKeyOfUser
        `shouldReturn` NoPublicKeyFound

    it "user needs to have ssh-ed25519 public key exposed"
        $ do
            let respKey = "ssh-rsa AAAAAAAA"
                nonEd25519PubKeyOfUser _ = pure [respKey]
                user = Username "user1"
                pubkey = PublicKeyHash ""
            inspectPublicKeyTemplate
                user
                pubkey
                nonEd25519PubKeyOfUser
        `shouldReturn` NoEd25519KeyFound

    it "user needs to the expected ssh-ed25519 public key exposed"
        $ do
            let respKey = "ssh-ed25519 AAAAAAAA"
                noExpectedEd25519PubKeyOfUser _ = pure [respKey]
                user = Username "user1"
                pubkey = PublicKeyHash "XAAAAAAY"
            inspectPublicKeyTemplate
                user
                pubkey
                noExpectedEd25519PubKeyOfUser
        `shouldReturn` NoEd25519KeyMatch

    it "user needs gets the expected ssh-ed25519 public key exposed 1"
        $ do
            let respKey = "ssh-ed25519 XAAAAAAY"
                okExpectedEd25519PubKeyOfUser _ = pure [respKey]
                user = Username "user1"
                pubkey = PublicKeyHash "XAAAAAAY"
            inspectPublicKeyTemplate
                user
                pubkey
                okExpectedEd25519PubKeyOfUser
        `shouldReturn` PublicKeyValidated

    it "user needs gets the expected ssh-ed25519 public key exposed 1"
        $ do
            let respKey1 = "ssh-ed25519 XAAAAAAY"
                respKey2 = "ssh-ed25519 AAAAAAAA"
                respKey3 = "ssh-rsa XXXXXXXXXXXXXXXXXXXXXXx"
                okExpectedEd25519PubKeyOfUser _ = pure [respKey1, respKey2, respKey3]
                user = Username "user1"
                pubkey = PublicKeyHash "XAAAAAAY"
            inspectPublicKeyTemplate
                user
                pubkey
                okExpectedEd25519PubKeyOfUser
        `shouldReturn` PublicKeyValidated

    it "should download CODEOWNERS file from repo with main" $ do
        downloadCodeownersFile
            (Repository "cardano-foundation" "hal-fixture-sin")
            `shouldReturn` ResponseCodeownersFile "antithesis: @notunrandom @cfhal\n"

    it "should download CODEOWNERS file from repo with master" $ do
        downloadCodeownersFile
            (Repository "cardano-foundation" "hal-fixture-cos")
            `shouldReturn` ResponseCodeownersFile "* @notunrandom\n"

    it "should download CODEOWNERS file from repo with trunk" $ do
        downloadCodeownersFile
            (Repository "cardano-foundation" "hal-fixture-tan")
            `shouldReturn` ResponseCodeownersFile "* @notunrandom\n"

    it "should throw if missing CODEOWNERS file" $ do
        downloadCodeownersFile
            (Repository "cardano-foundation" "hal-fixture-sec")
            `shouldThrow` anyException

    it "CODEOWNERS does not have role entry" $ do
        let noRoleEntry _ =
                pure
                    $ ResponseCodeownersFile
                    $ BS.fromStrict
                    $ BC.pack
                    $ unlines
                        [ "# Haskell components"
                        , "core        /     @user"
                        , "command-line/     @user"
                        ]
            user = Username "user1"
            repo = Repository "org" "repo"
        inspectRepoRoleForUserTemplate user repo noRoleEntry
            `shouldReturn` NoRoleEntryInCodeowners

    it "CODEOWNERS does not have users assigned" $ do
        let noRoleEntry _ =
                pure
                    $ ResponseCodeownersFile
                    $ BS.fromStrict
                    $ BC.pack
                    $ unlines
                        [ "# Haskell components"
                        , "core        /     @user"
                        , "command-line/     @user"
                        , ""
                        , "antithesis:"
                        ]
            user = Username "user1"
            repo = Repository "org" "repo"
        inspectRepoRoleForUserTemplate user repo noRoleEntry
            `shouldReturn` NoUsersAssignedToRoleInCodeowners

    it "CODEOWNERS does have other users assigned" $ do
        let noRoleEntry _ =
                pure
                    $ ResponseCodeownersFile
                    $ BS.fromStrict
                    $ BC.pack
                    $ unlines
                        [ "# Haskell components"
                        , "core        /     @user"
                        , "command-line/     @user"
                        , ""
                        , "antithesis: @user1 @user3"
                        ]
            user = Username "user2"
            repo = Repository "org" "repo"
        inspectRepoRoleForUserTemplate user repo noRoleEntry
            `shouldReturn` NoUserInCodeowners

    it "CODEOWNERS does have user assigned" $ do
        let noRoleEntry _ =
                pure
                    $ ResponseCodeownersFile
                    $ BS.fromStrict
                    $ BC.pack
                    $ unlines
                        [ "# Haskell components"
                        , "core        /     @user"
                        , "command-line/     @user"
                        , ""
                        , "antithesis: @user1 @user2 @user3"
                        ]
            user = Username "user2"
            repo = Repository "org" "repo"
        inspectRepoRoleForUserTemplate user repo noRoleEntry
            `shouldReturn` RepoRoleValidated

{-# LANGUAGE DuplicateRecordFields #-}

module Oracle.Validate.Requests.RegisterUserSpec (spec)
where

import Control.Monad (when)
import Core.Types.Basic
    ( Platform (..)
    , PublicKeyHash (..)
    , Username (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (toJSFact)
import Core.Types.Operation (Op (OpD, OpI), Operation (..))
import Data.Char (isAscii)
import Lib.SSH.Public
    ( SSHPublicKey (..)
    , encodeSSHPublicKey
    , extractPublicKeyHash
    )
import Oracle.Validate.Requests.RegisterUser
    ( RegisterUserFailure (..)
    , UnregisterUserFailure (..)
    , validateRegisterUser
    , validateUnregisterUser
    )
import Oracle.Validate.Requests.TestRun.Lib
    ( mkValidation
    )
import Oracle.Validate.Types
    ( AValidationResult (..)
    , Validated (..)
    , runValidate
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldReturn
    )
import Test.QuickCheck (Gen, arbitrary, suchThat)
import Test.QuickCheck.Crypton (sshGen)
import Test.QuickCheck.EGen (EGen, egenProperty, gen, genA, genBlind)
import Test.QuickCheck.Lib (withAPresence, withAPresenceInAList)
import User.Types (RegisterUserKey (..))
import Validation (KeyFailure (..))
import Validation.RegisterUser (PublicKeyFailure (..))

genUserDBElement :: Gen (Username, SSHPublicKey)
genUserDBElement = do
    user <- Username <$> arbitrary `suchThat` all isAscii
    pk <- SSHPublicKey <$> arbitrary `suchThat` all isAscii
    pure (user, pk)

genValidDBElement :: EGen (Username, SSHPublicKey)
genValidDBElement = do
    user <- Username <$> genA
    (_sign, pk) <- genBlind sshGen
    pure (user, encodeSSHPublicKey pk)

registerUserChange
    :: Platform
    -> Username
    -> PublicKeyHash
    -> Change RegisterUserKey (OpI ())
registerUserChange platform username pubkeyhash =
    Change
        { key =
            Key
                $ RegisterUserKey
                    { platform
                    , username
                    , pubkeyhash
                    }
        , operation = Insert ()
        }

unregisterUserChange
    :: Platform
    -> Username
    -> PublicKeyHash
    -> Change RegisterUserKey (OpD ())
unregisterUserChange platform username pubkeyhash =
    Change
        { key =
            Key
                $ RegisterUserKey
                    { platform
                    , username
                    , pubkeyhash
                    }
        , operation = Delete ()
        }

newtype OtherSSHPublicKey = OtherSSHPublicKey String
    deriving (Show, Eq)

genDBElementOther :: Gen (Username, OtherSSHPublicKey)
genDBElementOther = do
    user <- Username <$> arbitrary `suchThat` all isAscii
    pk <- arbitrary `suchThat` all isAscii
    pure (user, OtherSSHPublicKey $ "ssh-rsa " <> pk)

spec :: Spec
spec = do
    describe "validate requester requests" $ do
        it "validate a registered user" $ egenProperty $ do
            e@(user, pk) <- genValidDBElement
            let validation = mkValidation [] [] [] [e] []
                test =
                    validateRegisterUser validation
                        $ registerUserChange (Platform "github") user
                        $ extractPublicKeyHash pk
            pure $ runValidate test `shouldReturn` ValidationSuccess Validated

        it
            "fail to validate a registration of user for an unsupported platform"
            $ egenProperty
            $ do
                e@(user, pk) <- gen genUserDBElement
                db <- gen $ withAPresenceInAList 0.5 e genUserDBElement
                platform <- gen $ withAPresence 0.5 "github" arbitrary
                let validation = mkValidation [] [] [] db []
                    test =
                        validateRegisterUser validation
                            $ registerUserChange (Platform platform) user
                            $ extractPublicKeyHash pk
                pure
                    $ when (platform /= "github")
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RegisterUserPlatformNotSupported platform)

        it
            "fail to validate a registration if a user already registered within a given valid platform"
            $ egenProperty
            $ do
                e@(user, pk) <- gen genUserDBElement
                let platform = "github"
                    pubkey = extractPublicKeyHash pk
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , pubkeyhash = pubkey
                            }
                fact <- toJSFact registration ()
                let validation = mkValidation [fact] [] [] [e] []
                    test =
                        validateRegisterUser validation
                            $ registerUserChange (Platform platform) user pubkey
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RegisterUserKeyFailure (KeyAlreadyExists $ show registration))

        it
            "fail to validate a registration if there is no public key for a user"
            $ egenProperty
            $ do
                (user, pk) <- gen genUserDBElement
                let platform = "github"
                    pubkey = extractPublicKeyHash pk
                let validation = mkValidation [] [] [] [] []
                    test =
                        validateRegisterUser validation
                            $ registerUserChange (Platform platform) user pubkey
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (PublicKeyValidationFailure NoPublicKeyFound)

        it
            "fail to validate a registration if there is no ssh-ed25519 public key for a user"
            $ egenProperty
            $ do
                (user, OtherSSHPublicKey pk) <- gen genDBElementOther
                let platform = "github"
                    extractOtherPublicKeyHash sshPk =
                        let
                            expectedPrefix = "ssh-rsa " :: String
                        in
                            PublicKeyHash $ drop (length expectedPrefix) sshPk
                    pubkey = extractOtherPublicKeyHash pk
                    e = (user, SSHPublicKey pk)
                let validation = mkValidation [] [] [] [e] []
                    test =
                        validateRegisterUser validation
                            $ registerUserChange (Platform platform) user pubkey
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (PublicKeyValidationFailure NoEd25519KeyFound)

        it
            "fail to validate a registration if there is different ssh-ed25519 public key for a user"
            $ egenProperty
            $ do
                e@(user, pk1) <- genValidDBElement
                (_, pk2) <- genValidDBElement
                let platform = "github"
                    pubkey = extractPublicKeyHash pk2
                let validation = mkValidation [] [] [] [e] []
                    test =
                        validateRegisterUser validation
                            $ registerUserChange (Platform platform) user pubkey
                pure
                    $ when (pk1 /= pk2)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (PublicKeyValidationFailure NoEd25519KeyMatch)

        it
            "validate an unregistration if there is a given user already registered"
            $ egenProperty
            $ do
                (user, pk) <- gen genUserDBElement
                let platform = "github"
                    pubkey = extractPublicKeyHash pk
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , pubkeyhash = pubkey
                            }
                fact <- toJSFact registration ()
                let validation = mkValidation [fact] [] [] [] []
                    test =
                        validateUnregisterUser validation
                            $ unregisterUserChange (Platform platform) user pubkey
                pure $ runValidate test `shouldReturn` ValidationSuccess Validated

        it
            "fail to validate an unregistration if there is no a given user already registered"
            $ egenProperty
            $ do
                (user, pk) <- gen genUserDBElement
                (userOther, _) <- gen genUserDBElement
                let platform = "github"
                    pubkey = extractPublicKeyHash pk
                    registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , pubkeyhash = pubkey
                            }
                fact <- toJSFact registration ()
                let validation = mkValidation [fact] [] [] [] []
                    test =
                        validateUnregisterUser validation
                            $ unregisterUserChange (Platform platform) userOther pubkey
                    registrationOther =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = userOther
                            , pubkeyhash = pubkey
                            }
                pure
                    $ when (user /= userOther)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (UnregisterUserKeyFailure (KeyDoesNotExist $ show registrationOther))

        it
            "fail to validate an unregistration of user for an unsupported platform"
            $ egenProperty
            $ do
                (user, pk) <- gen genUserDBElement
                platform <-
                    gen $ withAPresence 0.5 "github" arbitrary `suchThat` all isAscii
                let registration =
                        RegisterUserKey
                            { platform = Platform platform
                            , username = user
                            , pubkeyhash = pubkey
                            }
                    pubkey = extractPublicKeyHash pk
                fact <- toJSFact registration ()
                let validation = mkValidation [fact] [] [] [] []
                    test =
                        validateUnregisterUser validation
                            $ unregisterUserChange (Platform platform) user pubkey
                pure
                    $ when (platform /= "github")
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (UnregisterUserPlatformNotSupported platform)

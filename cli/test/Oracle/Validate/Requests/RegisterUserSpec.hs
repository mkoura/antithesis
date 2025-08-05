{-# LANGUAGE DuplicateRecordFields #-}

module Oracle.Validate.Requests.RegisterUserSpec (spec)
where

import Control.Monad (when)
import Core.Types.Basic
    ( Platform (..)
    , PublicKeyHash
    , Username (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Operation (Op (OpI), Operation (..))
import Lib.SSH.Public
    ( SSHPublicKey (..)
    , encodeSSHPublicKey
    , extractPublicKeyHash
    )
import Oracle.Validate.Requests.RegisterUser
    ( RegisterUserFailure (..)
    , validateRegisterUser
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
import Test.QuickCheck (Gen, arbitrary)
import Test.QuickCheck.Crypton (sshGen)
import Test.QuickCheck.EGen (EGen, egenProperty, gen, genA, genBlind)
import Test.QuickCheck.Lib (withAPresence, withAPresenceInAList)
import User.Types (RegisterUserKey (..))

genUserDBElement :: Gen (Username, SSHPublicKey)
genUserDBElement = do
    user <- Username <$> arbitrary
    pk <- SSHPublicKey <$> arbitrary
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

spec :: Spec
spec = do
    describe "validate agent requests" $ do
        it "validate a registered user" $ egenProperty $ do
            e@(user, pk) <- genValidDBElement
            let validation = mkValidation [] [] [] [e] []
                test =
                    validateRegisterUser validation
                        $ registerUserChange (Platform "github") user
                        $ extractPublicKeyHash pk
            pure $ runValidate test `shouldReturn` ValidationSuccess Validated

        it "fail to validate a user with unsupported platform" $ egenProperty $ do
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

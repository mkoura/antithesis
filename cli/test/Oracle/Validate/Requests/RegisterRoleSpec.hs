{-# LANGUAGE DuplicateRecordFields #-}

module Oracle.Validate.Requests.RegisterRoleSpec (spec)
where

import Control.Monad (when)
import Core.Types.Basic
    ( Platform (..)
    , Repository (..)
    , Username (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Operation (Op (OpI), Operation (..))
import Oracle.Validate.Requests.RegisterRole
    ( RegisterRoleFailure (..)
    , validateRegisterRole
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
import Test.QuickCheck.EGen (egenProperty, gen)
import Test.QuickCheck.Lib (withAPresence, withAPresenceInAList)
import User.Types (RegisterRoleKey (..))

genRoleDBElement :: Gen (Username, Repository)
genRoleDBElement = do
    user <- Username <$> arbitrary
    repo <- Repository <$> arbitrary <*> arbitrary
    pure (user, repo)

registerRoleChange
    :: Platform
    -> Username
    -> Repository
    -> Change RegisterRoleKey (OpI ())
registerRoleChange platform user repo =
    Change
        { key =
            Key
                $ RegisterRoleKey
                    { platform = platform
                    , username = user
                    , repository = repo
                    }
        , operation = Insert ()
        }

spec :: Spec
spec = do
    describe "validate requester requests" $ do
        it "validate a registered role" $ egenProperty $ do
            e@(user, repo) <- gen genRoleDBElement
            let validation = mkValidation [] [] [] [] [e]
                test =
                    validateRegisterRole validation
                        $ registerRoleChange (Platform "github") user repo
            pure $ runValidate test `shouldReturn` ValidationSuccess Validated

        it "fail to validate a role for an unsupported platform" $ egenProperty $ do
            e@(user, repo) <- gen genRoleDBElement
            db <- gen $ withAPresenceInAList 0.5 e genRoleDBElement
            platform <- gen $ withAPresence 0.5 "github" arbitrary
            let validation = mkValidation [] [] [] [] db
                test =
                    validateRegisterRole validation
                        $ registerRoleChange (Platform platform) user repo
            pure
                $ when (platform /= "github")
                $ runValidate test
                `shouldReturn` ValidationFailure
                    (RegisterRolePlatformNotSupported platform)

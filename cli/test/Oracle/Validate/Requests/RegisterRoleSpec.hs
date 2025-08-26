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
import Core.Types.Fact (toJSFact)
import Core.Types.Operation (Op (OpD, OpI), Operation (..))
import Data.Char (isAscii)
import Oracle.Validate.Requests.RegisterRole
    ( RegisterRoleFailure (..)
    , UnregisterRoleFailure (..)
    , validateRegisterRole
    , validateUnregisterRole
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
import Test.QuickCheck.EGen (egenProperty, gen)
import Test.QuickCheck.Lib (withAPresence, withAPresenceInAList)
import User.Types (RegisterRoleKey (..))
import Validation (KeyFailure (..))
import Validation.RegisterRole
    ( RepositoryRoleFailure (..)
    )

genRoleDBElement :: Gen (Username, Repository)
genRoleDBElement = do
    user <- Username <$> arbitrary `suchThat` all isAscii
    repo <-
        Repository
            <$> arbitrary `suchThat` all isAscii
            <*> arbitrary `suchThat` all isAscii
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

unregisterRoleChange
    :: Platform
    -> Username
    -> Repository
    -> Change RegisterRoleKey (OpD ())
unregisterRoleChange platform user repo =
    Change
        { key =
            Key
                $ RegisterRoleKey
                    { platform = platform
                    , username = user
                    , repository = repo
                    }
        , operation = Delete ()
        }

spec :: Spec
spec = do
    describe "validate requester requests" $ do
        it "validate a registered role" $ egenProperty $ do
            e@(user, repo) <- gen genRoleDBElement
            let validation = mkValidation [] [] [] [] [e] [] [] []
                test =
                    validateRegisterRole validation
                        $ registerRoleChange (Platform "github") user repo
            pure $ runValidate test `shouldReturn` ValidationSuccess Validated

        it "fail to validate a role for an unsupported platform" $ egenProperty $ do
            e@(user, repo) <- gen genRoleDBElement
            db <- gen $ withAPresenceInAList 0.5 e genRoleDBElement
            platform <- gen $ withAPresence 0.5 "github" arbitrary
            let validation = mkValidation [] [] [] [] db [] [] []
                test =
                    validateRegisterRole validation
                        $ registerRoleChange (Platform platform) user repo
            pure
                $ when (platform /= "github")
                $ runValidate test
                `shouldReturn` ValidationFailure
                    (RegisterRolePlatformNotSupported platform)

        it
            "fail to validate a role registration if a user is already registered within a given valid platform"
            $ egenProperty
            $ do
                e@(user, repo) <- gen genRoleDBElement
                let platform = "github"
                    registration =
                        RegisterRoleKey
                            { platform = Platform platform
                            , username = user
                            , repository = repo
                            }
                fact <- toJSFact registration ()
                let validation = mkValidation [fact] [] [] [] [e] [] [] []
                    test =
                        validateRegisterRole validation
                            $ registerRoleChange (Platform platform) user repo
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RegisterRoleKeyFailure (KeyAlreadyExists $ show registration))

        it
            "fail to validate a role registration if there is no repo for a user present"
            $ egenProperty
            $ do
                (user, repo) <- gen genRoleDBElement
                let platform = "github"
                    validation = mkValidation [] [] [] [] [] [] [] []
                    test =
                        validateRegisterRole validation
                            $ registerRoleChange (Platform platform) user repo
                pure
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RoleNotPresentOnPlatform NoRoleEntryInCodeowners)

        it
            "fail to validate a role registration if there is different repo-user pair 1"
            $ egenProperty
            $ do
                e@(user, repo) <- gen genRoleDBElement
                (_, repo1) <- gen genRoleDBElement
                let platform = "github"
                    validation = mkValidation [] [] [] [] [e] [] [] []
                    test =
                        validateRegisterRole validation
                            $ registerRoleChange (Platform platform) user repo1
                pure
                    $ when (repo /= repo1)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RoleNotPresentOnPlatform NoRoleEntryInCodeowners)

        it
            "fail to validate a role registration if there is different repo-user pair 1"
            $ egenProperty
            $ do
                e@(user, repo) <- gen genRoleDBElement
                (user1, repo1) <- gen genRoleDBElement
                let platform = "github"
                    validation = mkValidation [] [] [] [] [e] [] [] []
                    test =
                        validateRegisterRole validation
                            $ registerRoleChange (Platform platform) user1 repo1
                pure
                    $ when (repo /= repo1 && user /= user1)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (RoleNotPresentOnPlatform NoRoleEntryInCodeowners)

        it
            "validate a role unregistration if there is a given user already registered"
            $ egenProperty
            $ do
                e@(user, repo) <- gen genRoleDBElement
                let platform = "github"
                    registration =
                        RegisterRoleKey
                            { platform = Platform platform
                            , username = user
                            , repository = repo
                            }
                fact <- toJSFact registration ()
                let validation = mkValidation [fact] [] [] [] [e] [] [] []
                    test =
                        validateUnregisterRole validation
                            $ unregisterRoleChange (Platform platform) user repo
                pure $ runValidate test `shouldReturn` ValidationSuccess Validated

        it
            "fail to validate a role unregistration if there is no a given user' role already registered"
            $ egenProperty
            $ do
                (user, repo) <- gen genRoleDBElement
                (userOther, _) <- gen genRoleDBElement
                let platform = "github"
                    registration =
                        RegisterRoleKey
                            { platform = Platform platform
                            , username = user
                            , repository = repo
                            }
                fact <- toJSFact registration ()
                let validation = mkValidation [fact] [] [] [] [] [] [] []
                    test =
                        validateUnregisterRole validation
                            $ unregisterRoleChange (Platform platform) userOther repo
                    registrationOther =
                        RegisterRoleKey
                            { platform = Platform platform
                            , username = userOther
                            , repository = repo
                            }
                pure
                    $ when (user /= userOther)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (UnregisterRoleKeyFailure (KeyDoesNotExist $ show registrationOther))

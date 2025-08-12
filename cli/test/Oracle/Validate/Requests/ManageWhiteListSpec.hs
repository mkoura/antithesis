{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Oracle.Validate.Requests.ManageWhiteListSpec (spec)
where

import Control.Monad (unless, when)
import Core.Types.Basic
    ( Platform (..)
    , Repository (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (toJSFact)
import Core.Types.Operation (Op (OpD, OpI), Operation (..))
import Oracle.Validate.Requests.ManageWhiteList
    ( UpdateWhiteListFailure (..)
    , validateAddWhiteListed
    , validateRemoveWhiteListed
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
import Test.QuickCheck
    ( Testable (..)
    , oneof
    )
import Test.QuickCheck.EGen (egenProperty, gen)
import Test.QuickCheck.Lib (withAPresence, withAPresenceInAList)
import Test.QuickCheck.Property (forAll)
import User.Agent.Types (WhiteListKey (..))
import User.Agent.TypesSpec (genAscii, genRepository)
import Validation (KeyFailure (..))

addWhiteListKey
    :: Platform
    -> Repository
    -> Change WhiteListKey (OpI ())
addWhiteListKey platform repo =
    Change
        { key = Key $ WhiteListKey platform repo
        , operation = Insert ()
        }

removeWhiteListKey
    :: Platform
    -> Repository
    -> Change WhiteListKey (OpD ())
removeWhiteListKey platform repo =
    Change
        { key = Key $ WhiteListKey platform repo
        , operation = Delete ()
        }

spec :: Spec
spec = do
    describe "validation of whitelist management" $ do
        it "validates a repository insertion" $ property $ do
            forAll genRepository $ \repo -> do
                let validation = mkValidation [] [] [] [] [] [repo]
                    test =
                        validateAddWhiteListed validation
                            $ addWhiteListKey (Platform "github") repo
                runValidate test `shouldReturn` ValidationSuccess Validated

        it "validates a repository insertion" $ egenProperty $ do
            repo <- gen genRepository
            let validation = mkValidation [] [] [] [] [] [repo]
                test =
                    validateAddWhiteListed validation
                        $ addWhiteListKey (Platform "github") repo
            pure $ runValidate test `shouldReturn` ValidationSuccess Validated

        it
            "fails to validate an insertion for a platform different from GitHub"
            $ egenProperty
            $ do
                repo <- gen genRepository
                platform <- gen $ withAPresence 0.5 "github" genAscii
                let validation = mkValidation [] [] [] [] [] [repo]
                    test =
                        validateAddWhiteListed validation
                            $ addWhiteListKey (Platform platform) repo
                pure
                    $ when (platform /= "github")
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (WhiteListPlatformUnsupported $ Platform platform)

        it
            "fails to validate a repository insertion if the repository is not in github"
            $ egenProperty
            $ do
                repo <- gen genRepository
                presence <- gen $ withAPresenceInAList 0.5 repo genRepository
                let validation = mkValidation [] [] [] [] [] presence
                    test =
                        validateAddWhiteListed validation
                            $ addWhiteListKey (Platform "github") repo
                pure
                    $ unless (repo `elem` presence)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (WhiteListRepositoryNotInThePlatform repo)
        it
            "fails to validate a repository insertion if the repository is already whitelisted"
            $ egenProperty
            $ do
                repo <- gen genRepository
                presenceInGithub <- gen $ withAPresenceInAList 0.5 repo genRepository
                let platform = Platform "github"
                    insertion = addWhiteListKey platform repo
                    key = WhiteListKey platform repo
                fact <- toJSFact key ()
                presenceInFacts <-
                    gen $ oneof [pure [], pure [fact]]
                let validation = mkValidation presenceInFacts [] [] [] [] presenceInGithub
                    test = validateAddWhiteListed validation insertion
                pure
                    $ when (fact `elem` presenceInFacts)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        ( WhiteListRepositoryKeyValidation
                            (KeyAlreadyExists $ show key)
                        )

        it "validates a repository removal" $ egenProperty $ do
            repo <- gen genRepository
            let platform = Platform "github"
                removal = removeWhiteListKey platform repo
                key = WhiteListKey platform repo
            fact <- toJSFact key ()
            presenceInGithub <- gen $ withAPresenceInAList 0.5 repo genRepository
            let validation = mkValidation [fact] [] [] [] [] presenceInGithub
                test = validateRemoveWhiteListed validation removal
            pure $ runValidate test `shouldReturn` ValidationSuccess Validated

        it
            "fail to validate a repository removal if the repository is not whitelisted"
            $ egenProperty
            $ do
                repo <- gen genRepository
                presenceInGithub <- gen $ withAPresenceInAList 0.5 repo genRepository
                let platform = Platform "github"
                    removal = removeWhiteListKey platform repo
                    key = WhiteListKey platform repo
                fact <- toJSFact key ()
                presenceInFacts <-
                    gen $ oneof [pure [], pure [fact]]
                let validation = mkValidation presenceInFacts [] [] [] [] presenceInGithub
                    test = validateRemoveWhiteListed validation removal
                pure
                    $ unless (fact `elem` presenceInFacts)
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        ( WhiteListRepositoryKeyValidation
                            (KeyDoesNotExist $ show key)
                        )
        it
            "fail to validate a repository removal if the platform is not GitHub"
            $ egenProperty
            $ do
                repo <- gen genRepository
                platform <- gen $ withAPresence 0.5 "github" genAscii
                let removal = removeWhiteListKey (Platform platform) repo
                    key = WhiteListKey (Platform platform) repo
                fact <- toJSFact key ()
                presenceInGithub <- gen $ withAPresenceInAList 0.5 repo genRepository
                let validation = mkValidation [fact] [] [] [] [] presenceInGithub
                    test = validateRemoveWhiteListed validation removal
                pure
                    $ when (platform /= "github")
                    $ runValidate test
                    `shouldReturn` ValidationFailure
                        (WhiteListPlatformUnsupported $ Platform platform)

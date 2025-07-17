{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Requests.TestRun.OthersSpec (spec)
where

import Core.Types.Basic
    ( Commit (Commit)
    , Directory (Directory)
    , Duration (..)
    , Platform (Platform)
    , PublicKeyHash
    , Repository (Repository, organization, project)
    , Try (Try)
    , Username (Username)
    )
import Core.Types.Fact (Fact (..), JSFact, parseFacts)
import Data.ByteString.Lazy.Char8 qualified as BL
import Lib.SSH.Public (encodePublicKey)
import Oracle.Validate.Requests.TestRun.Others
    ( AgentRejection (..)
    , validateToDoneCore
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( ASCIIString (..)
    , Arbitrary (..)
    , Gen
    , Positive (..)
    , Testable (..)
    , forAll
    , forAllBlind
    )
import Test.QuickCheck.Commit (CommitValue (..))
import Test.QuickCheck.Crypton (sshGen)
import Test.QuickCheck.Same
    ( isTheSame
    , theSame
    , tryDifferent
    )
import Text.JSON.Canonical (ToJSON (..), renderCanonicalJSON)
import User.Types
    ( RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunRejection (BrokenInstructions)
    , TestRunState (..)
    )
import Validation (Validation (..))

mkValidation
    :: Monad m
    => [JSFact]
    -> [(Repository, Commit)]
    -> [(Repository, Commit, Directory)]
    -> Validation m
mkValidation fs rs ds =
    Validation
        { mpfsGetFacts = do
            db <- toJSON fs
            return $ parseFacts db
        , githubCommitExists = \repository commit ->
            return $ (repository, commit) `elem` rs
        , githubDirectoryExists = \repository commit dir ->
            return $ (repository, commit, dir) `elem` ds
        }

registerRole :: Monad m => TestRun -> m JSFact
registerRole testRun = do
    key <-
        toJSON
            $ RegisterRoleKey
                { platform = testRun.platform
                , repository = testRun.repository
                , username = testRun.requester
                }
    value <- toJSON ()
    return $ Fact key value

registerUser :: (Monad m) => TestRun -> PublicKeyHash -> m JSFact
registerUser testRun pubkeyhash = do
    key <-
        toJSON
            $ RegisterUserKey
                { platform = testRun.platform
                , username = testRun.requester
                , pubkeyhash
                }
    value <- toJSON ()
    return $ Fact key value

testRunGen :: Gen TestRun
testRunGen = do
    ASCIIString platform <- arbitrary
    ASCIIString organization <- arbitrary
    ASCIIString project <- arbitrary
    ASCIIString directory <- arbitrary
    CommitValue commitId <- arbitrary
    Positive tryIndex <- arbitrary
    ASCIIString username <- arbitrary
    pure
        TestRun
            { platform = Platform platform
            , repository =
                Repository
                    { organization = organization
                    , project = project
                    }
            , directory = Directory directory
            , commitId = Commit commitId
            , tryIndex = Try tryIndex
            , requester = Username username
            }

spec :: Spec
spec = do
    describe "validate agent requests" $ do
        it "validate a reject test run"
            $ property
            $ forAll testRunGen
            $ \testRun -> do
                forAllBlind sshGen $ \(sign, pk) -> do
                    user <-
                        registerUser testRun $ encodePublicKey pk
                    role <-
                        registerRole testRun

                    testRunJ <- toJSON testRun
                    let pendingState =
                            Pending (Duration 5)
                                $ sign
                                $ BL.unpack
                                $ renderCanonicalJSON testRunJ
                    pendingStateJ <- toJSON pendingState
                    let testRunFact =
                            Fact
                                { factKey = testRunJ
                                , factValue = pendingStateJ
                                }
                        validation =
                            mkValidation
                                [user, role, testRunFact]
                                []
                                []
                        newTestRunState =
                            Rejected
                                pendingState
                                [BrokenInstructions]
                    mresult <-
                        validateToDoneCore
                            validation
                            testRun
                            newTestRunState
                    mresult `shouldBe` Nothing
        it "fail to validate a reject for a non-existing test run"
            $ property
            $ forAll testRunGen
            $ \testRun -> do
                forAllBlind sshGen $ \(sign, pk) -> do
                    user <-
                        registerUser testRun $ encodePublicKey pk
                    role <-
                        registerRole testRun

                    testRunJ <- toJSON testRun
                    let pendingState =
                            Pending (Duration 5)
                                $ sign
                                $ BL.unpack
                                $ renderCanonicalJSON testRunJ
                    let validation =
                            mkValidation
                                [user, role]
                                []
                                []
                        newTestRunState =
                            Rejected
                                pendingState
                                [BrokenInstructions]
                    mresult <-
                        validateToDoneCore
                            validation
                            testRun
                            newTestRunState
                    mresult `shouldBe` Just PreviousStateWrong
        it
            "fail to validate a reject for a pending test run with different state"
            $ property
            $ \duration ->
                forAll testRunGen $ \testRun ->
                    forAllBlind sshGen $ \(sign, pk) -> do
                        user <-
                            registerUser testRun $ encodePublicKey pk
                        role <-
                            registerRole testRun

                        testRunJ <- toJSON testRun
                        let pendingState =
                                Pending (Duration $ theSame duration)
                                    $ sign
                                    $ BL.unpack
                                    $ renderCanonicalJSON testRunJ
                            differentPendingState =
                                Pending (Duration $ tryDifferent duration)
                                    $ sign
                                    $ BL.unpack
                                    $ renderCanonicalJSON testRunJ
                        pendingStateJ <- toJSON pendingState
                        let testRunFact =
                                Fact
                                    { factKey = testRunJ
                                    , factValue = pendingStateJ
                                    }
                            validation =
                                mkValidation
                                    [user, role, testRunFact]
                                    []
                                    []
                            newTestRunState =
                                Rejected
                                    differentPendingState
                                    [BrokenInstructions]
                        mresult <-
                            validateToDoneCore
                                validation
                                testRun
                                newTestRunState
                        if isTheSame duration
                            then
                                mresult `shouldBe` Nothing
                            else mresult `shouldBe` Just PreviousStateWrong

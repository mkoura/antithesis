{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Requests.TestRun.CreateSpec (spec)
where

import Control.Lens (set, (&))
import Core.Types
    ( Commit (Commit)
    , Directory (Directory)
    , Duration (..)
    , Fact (..)
    , Platform (Platform)
    , PublicKeyHash
    , Repository (Repository, organization, project)
    , Try (Try)
    , Username (Username)
    )
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Maybe (maybeToList)
import Lib.SSH.Public (encodePublicKey)
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Requests.TestRun.Create (validateCreateTestRun)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldContain
    , shouldNotContain
    )
import Test.QuickCheck
    ( ASCIIString (..)
    , Positive (..)
    , Testable (..)
    , cover
    , forAll
    , forAllBlind
    )
import Test.QuickCheck.Commit (CommitValue (..))
import Test.QuickCheck.Crypton (sshGen)
import Test.QuickCheck.JSString (JSStringValue (..))
import Test.QuickCheck.Lib (withAPresence, withNothing)
import Test.QuickCheck.Same (isTheSame, theSame, tryDifferent)
import Text.JSON.Canonical (ToJSON (..), renderCanonicalJSON)
import User.Types
    ( RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunRejection (..)
    , TestRunState (..)
    , commitIdL
    , directoryL
    , platformL
    , repositoryL
    , requesterL
    , tryIndexL
    )
import Validation (Validation (..))

noValidation :: Monad m => Validation m
noValidation = mkValidation [] [] []

mkValidation
    :: Monad m
    => [Fact]
    -> [(Repository, Commit)]
    -> [(Repository, Commit, Directory)]
    -> Validation m
mkValidation fs rs ds =
    Validation
        { mpfsGetFacts = return fs
        , githubCommitExists = \repository commit ->
            return $ (repository, commit) `elem` rs
        , githubDirectoryExists = \repository commit dir ->
            return $ (repository, commit, dir) `elem` ds
        }

registerRole
    :: Monad m => String -> String -> String -> String -> m Fact
registerRole platform organization project requester = do
    key <-
        toJSON
            $ RegisterRoleKey
                { platform = Platform platform
                , repository =
                    Repository
                        { organization
                        , project
                        }
                , username = Username requester
                }
    value <- toJSON ()
    return $ Fact{key, value}

registerUser :: Monad m => String -> String -> PublicKeyHash -> m Fact
registerUser platform username publicKeyHash = do
    key <-
        toJSON
            $ RegisterUserKey
                { platform = Platform platform
                , username = Username username
                , pubkeyhash = publicKeyHash
                }
    value <- toJSON ()
    return $ Fact{key, value}

registerTestRun
    :: Monad m
    => String
    -> String
    -> String
    -> String
    -> String
    -> Int
    -> String
    -> Int
    -> Ed25519.Signature
    -> m Fact
registerTestRun
    platform
    organization
    project
    directory
    commitId
    tryIndex
    requester
    duration
    signature = do
        key <-
            toJSON
                $ TestRun
                    { platform = Platform platform
                    , repository =
                        Repository
                            { organization
                            , project
                            }
                    , directory = Directory directory
                    , commitId = Commit commitId
                    , tryIndex = Try tryIndex
                    , requester = Username requester
                    }
        value <- toJSON (Pending (Duration duration) signature)
        return $ Fact{key, value}

emptyTestRun :: TestRun
emptyTestRun =
    TestRun
        { platform = Platform ""
        , repository =
            Repository
                { organization = ""
                , project = ""
                }
        , directory = Directory ""
        , commitId = Commit ""
        , tryIndex = Try 1
        , requester = Username ""
        }

testConfig :: TestRunValidationConfig
testConfig =
    TestRunValidationConfig
        { maxDuration = 6
        , minDuration = 1
        , sshKeySelector = ""
        }

shouldHaveReason :: (Show a, Eq a) => Maybe [a] -> a -> IO ()
shouldHaveReason Nothing _ = pure ()
shouldHaveReason (Just reasons) reason =
    reasons `shouldContain` [reason]

shouldNotHaveReason :: (Show a, Eq a) => Maybe [a] -> a -> IO ()
shouldNotHaveReason Nothing _ = pure ()
shouldNotHaveReason (Just reasons) reason =
    reasons `shouldNotContain` [reason]

onConditionHaveReason
    :: (Show a, Eq a) => Maybe [a] -> a -> Bool -> IO ()
onConditionHaveReason result reason = \case
    True -> shouldHaveReason result reason
    False -> shouldNotHaveReason result reason

spec :: Spec
spec = do
    describe "validateRequest" $ do
        it "accepts valid test run" $ do
            property
                $ \(ASCIIString platform)
                   (ASCIIString organization)
                   (ASCIIString project)
                   (ASCIIString directory)
                   (CommitValue commitId)
                   (Positive tryIndex)
                   (ASCIIString username) ->
                        forAllBlind sshGen $ \(sign, pk) -> do
                            user <-
                                registerUser
                                    platform
                                    username
                                    $ encodePublicKey pk
                            role <-
                                registerRole
                                    platform
                                    organization
                                    project
                                    username
                            let mkTestRun j =
                                    emptyTestRun
                                        & set platformL (Platform platform)
                                        & set
                                            repositoryL
                                            ( Repository
                                                { organization = organization
                                                , project = project
                                                }
                                            )
                                        & set directoryL (Directory directory)
                                        & set commitIdL (Commit commitId)
                                        & set tryIndexL (Try j)
                                        & set requesterL (Username username)
                            let previous = case tryIndex of
                                    1 -> Nothing
                                    n -> do
                                        let previousTestRun = mkTestRun (n - 1)
                                        k <- toJSON previousTestRun
                                        v <-
                                            toJSON
                                                ( Pending (Duration 5)
                                                    $ sign
                                                    $ BL.unpack
                                                    $ renderCanonicalJSON k
                                                )
                                        Just $ Fact{key = k, value = v}
                            let validation =
                                    mkValidation
                                        ([user, role] <> maybeToList previous)
                                        [
                                            ( Repository
                                                { organization = organization
                                                , project = project
                                                }
                                            , Commit commitId
                                            )
                                        ]
                                        [
                                            ( Repository
                                                { organization = organization
                                                , project = project
                                                }
                                            , Commit commitId
                                            , Directory directory
                                            )
                                        ]
                            let testRun = mkTestRun tryIndex
                            testRunJ <- toJSON testRun
                            let testRunState =
                                    Pending (Duration 5)
                                        $ sign
                                        $ BL.unpack
                                        $ renderCanonicalJSON testRunJ
                            mresult <-
                                validateCreateTestRun
                                    testConfig
                                    validation
                                    testRun
                                    testRunState
                            mresult `shouldBe` Nothing

        it "reports unaccaptable duration"
            $ property
            $ \n message -> forAllBlind
                sshGen
                $ \(sign, _) -> do
                    let testRun = emptyTestRun
                        testRunState = Pending (Duration n) $ sign message
                    mresult <-
                        validateCreateTestRun testConfig noValidation testRun testRunState
                    onConditionHaveReason mresult UnacceptableDuration
                        $ n < minDuration testConfig || n > maxDuration testConfig
        it "reports unacceptable role"
            $ property
            $ \platform organization project username message ->
                forAllBlind sshGen $ \(sign, pk) -> do
                    let allTheSame =
                            isTheSame platform
                                && isTheSame organization
                                && isTheSame project
                                && isTheSame username
                        different = getJSStringValue . tryDifferent
                        same = getJSStringValue . theSame

                    cover 5 allTheSame "pass" $ do
                        do
                            user <-
                                registerUser
                                    (different platform)
                                    (different username)
                                    $ encodePublicKey pk
                            role <-
                                registerRole
                                    (different platform)
                                    (different organization)
                                    (different project)
                                    (different username)
                            let validation = mkValidation [user, role] [] []
                            let testRun =
                                    emptyTestRun
                                        & set platformL (Platform $ same platform)
                                        & set
                                            repositoryL
                                            ( Repository
                                                { organization = same organization
                                                , project = same project
                                                }
                                            )
                                        & set requesterL (Username $ same username)
                                testRunState = Pending (Duration 5) $ sign message
                            mresult <-
                                validateCreateTestRun
                                    testConfig
                                    validation
                                    testRun
                                    testRunState
                            onConditionHaveReason mresult UnacceptableRole
                                $ not allTheSame
        it "reports unacceptable try index" $ do
            let factIndexGen tryIndexRequest =
                    withNothing 0.8
                        $ withAPresence 0.2
                        $ tryIndexRequest + 1
            property
                $ \(JSStringValue platform)
                   (JSStringValue organization)
                   (JSStringValue project)
                   (JSStringValue directory)
                   (CommitValue commitId)
                   tryIndexRequest
                   (JSStringValue username)
                   duration
                   message -> forAllBlind sshGen $ \(sign, _pk) ->
                        forAll (factIndexGen tryIndexRequest) $ \mTryIndexFact -> do
                            let rightIndex = case mTryIndexFact of
                                    Just tryIndexFact ->
                                        tryIndexRequest == tryIndexFact + 1
                                    Nothing -> tryIndexRequest == 1
                            cover 5 rightIndex "pass" $ do
                                validation <- case mTryIndexFact of
                                    Just tryIndexFact -> do
                                        testRunFact <-
                                            registerTestRun
                                                platform
                                                organization
                                                project
                                                directory
                                                commitId
                                                tryIndexFact
                                                username
                                                duration
                                                $ sign message
                                        return $ mkValidation [testRunFact] [] []
                                    Nothing -> return noValidation
                                let testRun =
                                        emptyTestRun
                                            & set platformL (Platform platform)
                                            & set
                                                repositoryL
                                                ( Repository
                                                    { organization = organization
                                                    , project = project
                                                    }
                                                )
                                            & set directoryL (Directory directory)
                                            & set commitIdL (Commit commitId)
                                            & set tryIndexL (Try tryIndexRequest)
                                            & set requesterL (Username username)
                                    testRunState = Pending (Duration 5) $ sign message
                                mresult <-
                                    validateCreateTestRun
                                        testConfig
                                        validation
                                        testRun
                                        testRunState
                                onConditionHaveReason mresult UnacceptableTryIndex
                                    $ not rightIndex

        it "reports unacceptable directory" $ do
            let factDirectoryGen = withAPresence 0.3
            property
                $ \(JSStringValue platform)
                   (JSStringValue organization)
                   (JSStringValue project)
                   (JSStringValue directory)
                   (CommitValue commitId)
                   tryIndexRequest
                   (JSStringValue username)
                   duration
                   message -> forAllBlind sshGen
                        $ \(sign, _) ->
                            forAll (factDirectoryGen directory) $ \directoryFact -> do
                                let rightDirectory = directory == directoryFact
                                cover 5 rightDirectory "pass" $ do
                                    validation <- do
                                        testRunFact <-
                                            registerTestRun
                                                platform
                                                organization
                                                project
                                                directory
                                                commitId
                                                tryIndexRequest
                                                username
                                                duration
                                                $ sign message
                                        return
                                            $ mkValidation
                                                [testRunFact]
                                                []
                                                [
                                                    ( Repository organization project
                                                    , Commit commitId
                                                    , Directory directoryFact
                                                    )
                                                ]
                                    let testRun =
                                            emptyTestRun
                                                & set platformL (Platform platform)
                                                & set
                                                    repositoryL
                                                    ( Repository
                                                        { organization = organization
                                                        , project = project
                                                        }
                                                    )
                                                & set directoryL (Directory directory)
                                                & set commitIdL (Commit commitId)
                                                & set tryIndexL (Try tryIndexRequest)
                                                & set requesterL (Username username)
                                        testRunState = Pending (Duration 5) $ sign message
                                    mresult <-
                                        validateCreateTestRun
                                            testConfig
                                            validation
                                            testRun
                                            testRunState
                                    onConditionHaveReason mresult UnacceptableDirectory
                                        $ not rightDirectory

{-# LANGUAGE DuplicateRecordFields #-}

module User.Agent.Validation.RequestSpec (spec)
where

import Control.Lens (set, (&))
import Core.Types
    ( Directory (Directory)
    , Duration (..)
    , Fact (..)
    , Platform (Platform)
    , PublicKeyHash (PublicKeyHash)
    , Repository (Repository, organization, project)
    , SHA1 (SHA1)
    , Try (Try)
    , Username (Username)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldContain
    , shouldNotContain
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Positive (..)
    , Testable (..)
    , cover
    , elements
    , suchThat
    )
import Text.JSON.Canonical (ToJSON (..))
import User.Agent.Validation.Config (AgentValidationConfig (..))
import User.Agent.Validation.Request (validateRequest)
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
import Validation (Validation (Validation))

noFacts :: Monad m => Validation m
noFacts = Validation $ return []

validation :: Monad m => [Fact] -> Validation m
validation fs = Validation $ return fs

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

registerUser :: Monad m => String -> String -> String -> m Fact
registerUser platform username publicKeyHash = do
    key <-
        toJSON
            $ RegisterUserKey
                { platform = Platform platform
                , username = Username username
                , pubkeyhash = PublicKeyHash publicKeyHash
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
    -> m Fact
registerTestRun
    platform
    organization
    project
    directory
    commitId
    tryIndex
    requester
    duration = do
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
                    , commitId = SHA1 commitId
                    , tryIndex = Try tryIndex
                    , requester = Username requester
                    }
        value <- toJSON (Pending (Duration duration))
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
        , commitId = SHA1 ""
        , tryIndex = Try 1
        , requester = Username ""
        }

testConfig :: AgentValidationConfig
testConfig =
    AgentValidationConfig
        { maxDuration = 6
        , minDuration = 1
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

data Same a = Same a | NotSame a a
    deriving (Show, Eq)

tryDifferent :: Same a -> a
tryDifferent (Same a) = a
tryDifferent (NotSame _ a) = a

theSame :: Same a -> a
theSame (Same a) = a
theSame (NotSame a _) = a

ascii :: String
ascii = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> [' ', '-', '_']

newtype Ascii = Ascii Char
    deriving (Show, Eq)

instance Arbitrary Ascii where
    arbitrary = Ascii <$> elements ascii

asciiString :: [Ascii] -> String
asciiString = map (\(Ascii c) -> c)
instance (Eq a, Arbitrary a) => Arbitrary (Same a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary `suchThat` (/= a)
        elements [Same a, NotSame a b]

isTheSame :: Same a -> Bool
isTheSame (Same _) = True
isTheSame (NotSame _ _) = False

spec :: Spec
spec = do
    describe "validateRequest" $ do
        it "reports unaccaptable duration" $ property $ \n -> do
            let testRun = emptyTestRun
                testRunState = Pending (Duration n)
            mresult <- validateRequest testConfig noFacts testRun testRunState
            onConditionHaveReason mresult UnacceptableDuration
                $ n < minDuration testConfig || n > maxDuration testConfig
        it "reports unacceptable role"
            $ property
            $ \platform
               organization
               project
               username -> do
                    let allTheSame =
                            isTheSame platform
                                && isTheSame organization
                                && isTheSame project
                                && isTheSame username
                        different = asciiString . tryDifferent
                        same = asciiString . theSame

                    cover 5 allTheSame "pass" $ do
                        do
                            user <-
                                registerUser
                                    (different platform)
                                    (different username)
                                    "publicKeyHash"
                            role <-
                                registerRole
                                    (different platform)
                                    (different organization)
                                    (different project)
                                    (different username)
                            let facts = validation [user, role]
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
                                testRunState = Pending (Duration 5)
                            mresult <- validateRequest testConfig facts testRun testRunState
                            onConditionHaveReason mresult UnacceptableRole
                                $ not allTheSame
        it "reports unacceptable try index"
            $ property
            $ \platform
               organization
               project
               directory
               commitId
               mTryIndexFact
               (Positive tryIndexRequest)
               username
               duration -> do
                    let rightIndex = case mTryIndexFact of
                            Just ((Positive tryIndexFact)) ->
                                tryIndexRequest == tryIndexFact + 1
                            Nothing -> tryIndexRequest == 1
                    cover 5 rightIndex "pass" $ do
                        facts <- case mTryIndexFact of
                            Just (Positive tryIndexFact) -> do
                                testRunFact <-
                                    registerTestRun
                                        (asciiString platform)
                                        (asciiString organization)
                                        (asciiString project)
                                        (asciiString directory)
                                        (asciiString commitId)
                                        tryIndexFact
                                        (asciiString username)
                                        duration
                                return $ validation [testRunFact]
                            Nothing -> return noFacts
                        let testRun =
                                emptyTestRun
                                    & set platformL (Platform $ asciiString platform)
                                    & set
                                        repositoryL
                                        ( Repository
                                            { organization = asciiString organization
                                            , project = asciiString project
                                            }
                                        )
                                    & set directoryL (Directory $ asciiString directory)
                                    & set commitIdL (SHA1 $ asciiString commitId)
                                    & set tryIndexL (Try tryIndexRequest)
                                    & set requesterL (Username $ asciiString username)
                            testRunState = Pending (Duration 5)
                        mresult <- validateRequest testConfig facts testRun testRunState
                        onConditionHaveReason mresult UnacceptableTryIndex
                            $ not rightIndex

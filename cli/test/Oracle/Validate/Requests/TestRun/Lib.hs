{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Requests.TestRun.Lib
    ( jsFactRole
    , jsFactUser
    , mkValidation
    , noValidation
    , gitCommit
    , gitDirectory
    , signTestRun
    , changeTestRun
    , asciiStringL
    , positiveL
    , changePlatform
    , changeCommitId
    , changeDirectory
    , changeOrganization
    , changeProject
    , changeTry
    , changeRequester
    , testRunGen
    , signatureGen
    , testConfigGen
    , testConfigShrink
    , testConfigEGen
    , testRunEGen
    )
where

import Control.Lens
    ( Lens'
    , Wrapped (_Wrapped')
    , (&)
    , (.~)
    , (^.)
    )
import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , Platform (Platform)
    , PublicKeyHash
    , Repository (..)
    , Try (Try)
    , Username (Username)
    , organizationL
    , projectL
    )
import Core.Types.Fact
    ( Fact (Fact)
    , JSFact
    , parseFacts
    , toJSFact
    )
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Maybe (mapMaybe)
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Test.QuickCheck
    ( ASCIIString (..)
    , Arbitrary (..)
    , Gen
    , Positive (..)
    , suchThat
    )
import Test.QuickCheck.Commit (CommitValue (..))
import Test.QuickCheck.Crypton ()
import Test.QuickCheck.EGen (EGen, gen, genShrink)
import Text.JSON.Canonical
    ( FromJSON (fromJSON)
    , ToJSON (toJSON)
    , renderCanonicalJSON
    )
import User.Types
    ( RegisterRoleKey (RegisterRoleKey, platform, repository, username)
    , RegisterUserKey (RegisterUserKey, platform, pubkeyhash, username)
    , TestRun (..)
    , commitIdL
    , directoryL
    , platformL
    , repositoryL
    , requesterL
    , tryIndexL
    )
import Validation (Validation (..))
import Validation.RegisterRole (RepoRoleValidation (..))
import Validation.RegisterUser (PublicKeyValidation (..))

jsFactRole :: Monad m => TestRun -> m JSFact
jsFactRole testRun =
    toJSFact
        RegisterRoleKey
            { platform = testRun.platform
            , repository = testRun.repository
            , username = testRun.requester
            }
        ()

jsFactUser :: (Monad m) => TestRun -> PublicKeyHash -> m JSFact
jsFactUser testRun pubkeyhash =
    toJSFact
        RegisterUserKey
            { platform = testRun.platform
            , username = testRun.requester
            , pubkeyhash
            }
        ()

mkValidation
    :: Monad m
    => [JSFact]
    -> [(Repository, Commit)]
    -> [(Repository, Commit, Directory)]
    -> [(Username, PublicKeyHash)]
    -> [(Username, Repository)]
    -> Validation m
mkValidation fs rs ds upk rr =
    Validation
        { mpfsGetFacts = do
            db <- toJSON fs
            return $ parseFacts db
        , mpfsGetTestRuns =
            pure $ mapMaybe (\(Fact k _ :: JSFact) -> fromJSON k) fs
        , githubCommitExists = \repository commit ->
            return $ (repository, commit) `elem` rs
        , githubDirectoryExists = \repository commit dir ->
            return $ (repository, commit, dir) `elem` ds
        , githubUserPublicKeys = \username publicKey ->
            return
                $ if (username, publicKey) `elem` upk
                    then PublicKeyValidated
                    else NoEd25519KeyMatch
        , githubRepositoryRole = \username repository ->
            return
                $ if (username, repository) `elem` rr
                    then RepoRoleValidated
                    else NoRoleEntryInCodeowners
        }

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

testRunEGen :: EGen TestRun
testRunEGen = gen testRunGen

asciiStringL
    :: Functor f => (String -> f String) -> ASCIIString -> f ASCIIString
asciiStringL f s = ASCIIString <$> f (getASCIIString s)

positiveL :: Lens' (Positive Int) Int
positiveL f n = Positive <$> f (getPositive n)

changePlatform :: TestRun -> Gen TestRun
changePlatform = changeTestRun (platformL . _Wrapped') asciiStringL

changeCommitId :: TestRun -> Gen TestRun
changeCommitId = changeTestRun (commitIdL . _Wrapped') asciiStringL

changeDirectory :: TestRun -> Gen TestRun
changeDirectory = changeTestRun (directoryL . _Wrapped') asciiStringL

changeOrganization :: TestRun -> Gen TestRun
changeOrganization = changeTestRun (repositoryL . organizationL) asciiStringL

changeProject :: TestRun -> Gen TestRun
changeProject = changeTestRun (repositoryL . projectL) asciiStringL

changeTry :: TestRun -> Gen TestRun
changeTry = changeTestRun (tryIndexL . _Wrapped') positiveL

changeRequester :: TestRun -> Gen TestRun
changeRequester = changeTestRun (requesterL . _Wrapped') asciiStringL

changeTestRun
    :: (Arbitrary b, Eq a)
    => Lens' TestRun a
    -> Lens' b a
    -> TestRun
    -> Gen TestRun
changeTestRun l qc testRun = do
    let old = testRun ^. l
    new <- arbitrary `suchThat` \new -> new ^. qc /= old
    pure $ testRun & l .~ (new ^. qc)

noValidation :: Monad m => Validation m
noValidation = mkValidation [] [] [] [] []

gitCommit :: TestRun -> (Repository, Commit)
gitCommit testRun =
    ( testRun.repository
    , testRun.commitId
    )

gitDirectory :: TestRun -> (Repository, Commit, Directory)
gitDirectory testRun =
    ( testRun.repository
    , testRun.commitId
    , testRun.directory
    )

signTestRun :: (Monad m, ToJSON m a) => (String -> b) -> a -> m b
signTestRun sign testRun = do
    testRunJ <- toJSON testRun
    pure $ sign $ BL.unpack $ renderCanonicalJSON testRunJ

signatureGen :: Gen Ed25519.Signature
signatureGen = do
    pk <- Ed25519.generateSecretKey
    pure $ Ed25519.sign pk (Ed25519.toPublic pk) ("hello" :: ByteString)

testConfigGen :: Gen TestRunValidationConfig
testConfigGen = do
    maxDuration <- arbitrary
    Positive minDuration <- arbitrary `suchThat` (<= maxDuration)
    pure
        $ TestRunValidationConfig
            { maxDuration = getPositive maxDuration
            , minDuration
            }

testConfigShrink
    :: TestRunValidationConfig -> [TestRunValidationConfig]
testConfigShrink config
    | config.maxDuration == config.minDuration = []
    | otherwise =
        [ TestRunValidationConfig
            { maxDuration = config.maxDuration
            , minDuration = minDuration'
            }
        | minDuration' <- [config.minDuration + 1 .. config.maxDuration]
        ]

testConfigEGen :: EGen TestRunValidationConfig
testConfigEGen = genShrink testConfigGen testConfigShrink

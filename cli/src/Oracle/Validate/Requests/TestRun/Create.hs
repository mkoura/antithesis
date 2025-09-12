{-# LANGUAGE OverloadedRecordDot #-}

module Oracle.Validate.Requests.TestRun.Create
    ( validateCreateTestRun
    , validateCreateTestRunCore
    , TestRunRejection (..)
    , CreateTestRunFailure (..)
    ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic (Directory (..), Duration (..), Try (..))
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..))
import Core.Types.Operation (Op (..), Operation (..))
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (mapMaybe)
import Lib.GitHub (GithubResponseError, GithubResponseStatusCodeError)
import Lib.JSON.Canonical.Extra (object, stringJSON, (.=))
import Lib.SSH.Public (decodePublicKey)
import Oracle.Types (requestZooGetTestRunKey)
import Oracle.Validate.DownloadAssets
    ( AssetValidationFailure
    , validateAssets
    )
import Oracle.Validate.Requests.Lib (keyAlreadyPendingFailure)
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Types
    ( ForRole
    , Validate
    , Validated (..)
    , forUser
    , mapFailure
    , sequenceValidate
    , throwJusts
    )
import Text.JSON.Canonical
    ( ToJSON (..)
    , renderCanonicalJSON
    )
import User.Agent.Types (WhiteListKey (..))
import User.Types
    ( Phase (PendingT)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunState (..)
    , roleOfATestRun
    )
import Validation
    ( KeyFailure
    , Validation (..)
    , hoistValidation
    , insertValidation
    )

data CreateTestRunFailure
    = CreateTestRunRejections [TestRunRejection]
    | CreateTestRunKeyFailure KeyFailure
    | CreateTestConfigNotAvailable
    | CreateTestRunInvalidSSHKey
    | CreateTestRunKeyAlreadyPending TestRun
    deriving (Eq, Show)

instance Monad m => ToJSON m CreateTestRunFailure where
    toJSON (CreateTestRunRejections rejections) =
        object ["createTestRunRejections" .= rejections]
    toJSON (CreateTestRunKeyFailure keyFailure) =
        object ["createTestRunKeyFailure" .= keyFailure]
    toJSON CreateTestConfigNotAvailable =
        stringJSON "Token configuration is not available yet"
    toJSON CreateTestRunInvalidSSHKey =
        stringJSON "Invalid SSH key"
    toJSON (CreateTestRunKeyAlreadyPending testRun) =
        object ["createTestRunKeyAlreadyPending" .= testRun]

validateCreateTestRun
    :: Monad m
    => TestRunValidationConfig
    -> Validation m
    -> ForRole
    -> Change TestRun (OpI (TestRunState PendingT))
    -> Validate CreateTestRunFailure m Validated
validateCreateTestRun
    testRunConfig
    validation
    forRole
    change@(Change (Key testRun) (Insert testRunState)) = do
        when (forUser forRole)
            $ keyAlreadyPendingFailure
                validation
                CreateTestRunKeyAlreadyPending
                testRun
                requestZooGetTestRunKey
        mapFailure CreateTestRunKeyFailure
            $ insertValidation validation change
        mapFailure CreateTestRunRejections
            $ validateCreateTestRunCore
                testRunConfig
                validation
                testRun
                testRunState

data TestRunRejection
    = UnacceptableDuration
    | UnacceptableCommit
    | UnacceptableTryIndex
    | UnacceptableRole
    | NoRegisteredKeyVerifiesTheSignature
    | UserHasNoRegisteredSSHKeys
    | GithubResponseError GithubResponseError
    | GithubResponseStatusCodeError GithubResponseStatusCodeError
    | RepositoryNotWhitelisted
    | UnacceptableAssets AssetValidationFailure
    deriving (Eq, Show)

instance Monad m => ToJSON m TestRunRejection where
    toJSON UnacceptableDuration =
        stringJSON "unacceptable duration"
    toJSON UnacceptableCommit =
        stringJSON "unacceptable commit"
    toJSON UnacceptableTryIndex =
        stringJSON "unacceptable try index"
    toJSON UnacceptableRole =
        stringJSON "unacceptable role"
    toJSON NoRegisteredKeyVerifiesTheSignature =
        stringJSON "no registered key verifies the signature"
    toJSON UserHasNoRegisteredSSHKeys =
        stringJSON "user has no Ed25519 registered SSH keys"
    toJSON (GithubResponseError err) =
        object ["githubResponseError" .= err]
    toJSON (GithubResponseStatusCodeError err) =
        object ["githubResponseStatusCodeError" .= err]
    toJSON RepositoryNotWhitelisted =
        stringJSON "repository not whitelisted"
    toJSON (UnacceptableAssets failure) =
        object ["unacceptableAssets" .= failure]

checkDuration
    :: TestRunValidationConfig -> Duration -> Maybe TestRunRejection
checkDuration TestRunValidationConfig{maxDuration, minDuration} (Duration n)
    | n < minDuration || n > maxDuration = Just UnacceptableDuration
    | otherwise = Nothing

checkRole
    :: Monad m => Validation m -> TestRun -> m (Maybe TestRunRejection)
checkRole
    Validation{mpfsGetFacts}
    testRun = do
        fs <- mpfsGetFacts
        let roleFact = roleOfATestRun testRun
        if Fact roleFact () `elem` fs
            then return Nothing
            else return $ Just UnacceptableRole

checkWhiteList
    :: Monad m
    => Validation m
    -> TestRun
    -> m (Maybe TestRunRejection)
checkWhiteList
    Validation{mpfsGetFacts}
    testRun = do
        let proposed = WhiteListKey testRun.platform testRun.repository
        facts :: [Fact WhiteListKey ()] <- mpfsGetFacts
        if any (\(Fact k _) -> k == proposed) facts
            then return Nothing
            else return $ Just RepositoryNotWhitelisted

checkTryIndex
    :: Monad m
    => Validation m
    -> TestRun
    -> m (Maybe TestRunRejection)
checkTryIndex
    Validation{mpfsGetTestRuns}
    testRun = do
        testRuns :: [TestRun] <- mpfsGetTestRuns
        let sameCommitTestRuns =
                filter
                    ( \tr ->
                        tr.repository == testRun.repository
                            && commitId tr == commitId testRun
                            && tr.platform == testRun.platform
                            && directory tr == directory testRun
                    )
                    testRuns
            latest = case sameCommitTestRuns of
                [] -> Try 0
                _ -> maximum $ map tryIndex sameCommitTestRuns

        if tryIndex testRun == succ latest
            then return Nothing
            else return $ Just UnacceptableTryIndex

checkCommit
    :: Monad m
    => Validation m
    -> TestRun
    -> m (Maybe TestRunRejection)
checkCommit
    Validation{githubCommitExists}
    testRun = do
        existsE <- githubCommitExists testRun.repository (commitId testRun)
        pure $ case existsE of
            Left err -> Just $ GithubResponseError err
            Right exists ->
                if exists
                    then Nothing
                    else Just UnacceptableCommit

checkSignature
    :: Monad m
    => Validation m
    -> TestRun
    -> Ed25519.Signature
    -> m (Maybe TestRunRejection)
checkSignature
    Validation{mpfsGetFacts}
    testRun
    signature = do
        registeredUsers <- mpfsGetFacts @_ @()
        testRunJ <- toJSON testRun
        let
            userKeys =
                mapMaybe
                    (decodePublicKey . pubkeyhash)
                    . filter (\(RegisterUserKey _ u _) -> u == requester testRun)
                    $ factKey <$> registeredUsers
            load = BL.toStrict $ renderCanonicalJSON testRunJ
        if null userKeys
            then return $ Just UserHasNoRegisteredSSHKeys
            else
                if any ((\verify -> verify signature load) . fst) userKeys
                    then return Nothing
                    else return $ Just NoRegisteredKeyVerifiesTheSignature

validateCreateTestRunCore
    :: Monad m
    => TestRunValidationConfig
    -> Validation m
    -> TestRun
    -> TestRunState PendingT
    -> Validate [TestRunRejection] m Validated
validateCreateTestRunCore
    config
    validation
    testRun
    (Pending duration signature) = do
        (withSystemTempDirectory $ hoistValidation validation)
            "antithesis-test-run"
            $ \tmpDir -> do
                let liftValidate f = lift (f validation testRun) >>= throwJusts
                Validated
                    <$ sequenceValidate
                        [ throwJusts $ checkDuration config duration
                        , liftValidate checkRole
                        , liftValidate checkTryIndex
                        , liftValidate checkCommit
                        , mapFailure UnacceptableAssets
                            $ validateAssets (Directory tmpDir) validation testRun
                        , liftValidate checkWhiteList
                        , liftValidate $ \v t -> checkSignature v t signature
                        ]

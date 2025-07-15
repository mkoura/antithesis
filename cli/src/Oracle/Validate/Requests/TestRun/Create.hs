{-# LANGUAGE OverloadedRecordDot #-}

module Oracle.Validate.Requests.TestRun.Create
    ( validateCreateTestRun
    , validateCreateTestRunCore
    , TestRunRejection (..)
    ) where

import Core.Types
    ( Change (..)
    , Duration (..)
    , Fact (..)
    , Key (..)
    , Operation (..)
    , Try (..)
    , parseFacts
    )
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (catMaybes, mapMaybe)
import Lib.JSON (stringJSON)
import Lib.SSH.Public (decodePublicKey)
import Oracle.Types (Request (..))
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Types (ValidationResult (..))
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ReportSchemaErrors
    , ToJSON (..)
    , expectedButGotValue
    , fromJSString
    , renderCanonicalJSON
    )
import User.Types
    ( Phase (PendingT)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunState (..)
    , roleOfATestRun
    )
import Validation (Validation (..))

validateCreateTestRun
    :: Monad m
    => TestRunValidationConfig
    -> Validation m
    -> Request TestRun (TestRunState PendingT)
    -> m ValidationResult
validateCreateTestRun
    testRunConfig
    validation
    (Request _refId _owner (Change (Key testRun) operation)) =
        case operation of
            Insert state -> do
                result <-
                    validateCreateTestRunCore
                        testRunConfig
                        validation
                        testRun
                        state
                case result of
                    Nothing -> pure Validated
                    Just rejections ->
                        pure
                            $ CannotValidate
                            $ "test run validation failed for the following reasons: "
                                <> unwords (fmap show rejections)
            _ ->
                pure
                    $ CannotValidate
                        "only insert operation is supported for test runs"

data TestRunRejection
    = UnacceptableDuration
    | UnacceptableCommit
    | UnacceptableTryIndex
    | UnacceptableRole
    | UnacceptableDirectory
    | UnacceptableSignature
    | AnyReason String
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
    toJSON UnacceptableDirectory =
        stringJSON "unacceptable directory"
    toJSON UnacceptableSignature =
        stringJSON "unacceptable signature"
    toJSON (AnyReason reason) =
        stringJSON reason

instance ReportSchemaErrors m => FromJSON m TestRunRejection where
    fromJSON (JSString jsString) = do
        let reason = fromJSString jsString
        case reason of
            "unacceptable duration" -> pure UnacceptableDuration
            "unacceptable commit" -> pure UnacceptableCommit
            "unacceptable try index" -> pure UnacceptableTryIndex
            "unacceptable role" -> pure UnacceptableRole
            "unacceptable directory" -> pure UnacceptableDirectory
            "unacceptable signature" -> pure UnacceptableSignature
            _ -> pure $ AnyReason reason
    fromJSON other =
        expectedButGotValue "a string representing a reason" other
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
        roleFactKey <- toJSON roleFact
        roleFactValue <- toJSON ()
        if Fact roleFactKey roleFactValue `elem` fs
            then return Nothing
            else return $ Just UnacceptableRole

checkTryIndex
    :: Monad m
    => Validation m
    -> TestRun
    -> m (Maybe TestRunRejection)
checkTryIndex
    Validation{mpfsGetFacts}
    testRun = do
        fs <- mpfsGetFacts
        let testRuns :: [(TestRun, TestRunState PendingT)] = parseFacts fs
        let sameCommitTestRuns =
                filter
                    ( \(tr, _) ->
                        repository tr == repository testRun
                            && commitId tr == commitId testRun
                            && tr.platform == testRun.platform
                            && directory tr == directory testRun
                    )
                    testRuns
            latest = case sameCommitTestRuns of
                [] -> Try 0
                _ -> maximum $ map (tryIndex . fst) sameCommitTestRuns

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
        exists <- githubCommitExists (repository testRun) (commitId testRun)
        if exists
            then return Nothing
            else return $ Just UnacceptableCommit

checkDirectory
    :: Monad m
    => Validation m
    -> TestRun
    -> m (Maybe TestRunRejection)
checkDirectory
    Validation{githubDirectoryExists}
    testRun = do
        exists <-
            githubDirectoryExists
                (repository testRun)
                (commitId testRun)
                (directory testRun)
        if exists
            then return Nothing
            else return $ Just UnacceptableDirectory

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
        fs <- mpfsGetFacts
        testRunJ <- toJSON testRun
        let registeredUsers :: [(RegisterUserKey, ())] = parseFacts fs
            userKeys =
                mapMaybe
                    (decodePublicKey . pubkeyhash)
                    . filter (\(RegisterUserKey _ u _) -> u == requester testRun)
                    $ fst <$> registeredUsers
            load = BL.toStrict $ renderCanonicalJSON testRunJ
        if any (\verify -> verify signature load) userKeys
            then return Nothing
            else return $ Just UnacceptableSignature

validateCreateTestRunCore
    :: Monad m
    => TestRunValidationConfig
    -> Validation m
    -> TestRun
    -> TestRunState PendingT
    -> m (Maybe [TestRunRejection])
validateCreateTestRunCore
    config
    validation
    testRun
    (Pending duration signature) = do
        result <-
            catMaybes
                <$> sequence
                    [ pure $ checkDuration config duration
                    , checkRole validation testRun
                    , checkTryIndex validation testRun
                    , checkCommit validation testRun
                    , checkDirectory validation testRun
                    , checkSignature validation testRun signature
                    ]

        case result of
            [] -> return Nothing
            reasons -> return $ Just reasons

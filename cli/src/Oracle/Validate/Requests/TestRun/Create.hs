{-# LANGUAGE OverloadedRecordDot #-}

module Oracle.Validate.Requests.TestRun.Create
    ( validateCreateTestRun
    , validateCreateTestRunCore
    , TestRunRejection (..)
    , CreateTestRunFailure (..)
    , renderCreateTestRunFailure
    ) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic (Duration (..), Try (..))
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..))
import Core.Types.Operation (Op (..), Operation (..))
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (catMaybes, mapMaybe)
import Lib.JSON (stringJSON)
import Lib.SSH.Public (decodePublicKey)
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Types
    ( Validate
    , ValidationResult
    , mapFailure
    , notValidated
    , runValidate
    )
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
import Validation
    ( KeyFailure
    , Validation (..)
    , insertValidation
    , renderKeyFailure
    )

data CreateTestRunFailure
    = CreateTestRunRejections [TestRunRejection]
    | CreateTestRunKeyFailure KeyFailure
    deriving (Eq, Show)

renderCreateTestRunFailure :: CreateTestRunFailure -> String
renderCreateTestRunFailure (CreateTestRunRejections rejections) =
    "CreateTestRun failed with rejections: "
        ++ unwords (map show rejections)
renderCreateTestRunFailure (CreateTestRunKeyFailure keyFailure) =
    "CreateTestRun failed with key failure: "
        ++ renderKeyFailure keyFailure

validateCreateTestRun
    :: Monad m
    => TestRunValidationConfig
    -> Validation m
    -> Change TestRun (OpI (TestRunState PendingT))
    -> m (ValidationResult CreateTestRunFailure)
validateCreateTestRun
    testRunConfig
    validation
    change@(Change (Key testRun) (Insert testRunState)) = runValidate $ do
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
        if Fact roleFact () `elem` fs
            then return Nothing
            else return $ Just UnacceptableRole

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
                        repository tr == repository testRun
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
        registeredUsers :: [Fact RegisterUserKey ()] <- mpfsGetFacts
        testRunJ <- toJSON testRun
        let
            userKeys =
                mapMaybe
                    (decodePublicKey . pubkeyhash)
                    . filter (\(RegisterUserKey _ u _) -> u == requester testRun)
                    $ factKey <$> registeredUsers
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
    -> Validate [TestRunRejection] m ()
validateCreateTestRunCore
    config
    validation
    testRun
    (Pending duration signature) = do
        result <-
            lift
                $ catMaybes
                    <$> sequence
                        [ pure $ checkDuration config duration
                        , checkRole validation testRun
                        , checkTryIndex validation testRun
                        , checkCommit validation testRun
                        , checkDirectory validation testRun
                        , checkSignature validation testRun signature
                        ]
        unless (null result)
            $ notValidated result

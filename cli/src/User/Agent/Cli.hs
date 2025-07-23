{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module User.Agent.Cli
    ( AgentCommand (..)
    , TestRunId (..)
    , IsReady (..)
    , agentCmd
    , agentCmdCore
    )
where

import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic (Duration, Owner, TokenId)
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..), keyHash, parseFacts)
import Core.Types.Operation (Op (..), Operation (..))
import Core.Types.Tx (WithTxHash (..))
import Data.Functor (($>), (<&>))
import Data.List (find)
import MPFS.API
    ( MPFS (..)
    , RequestUpdateBody (..)
    )
import Oracle.Validate.Requests.TestRun.Update
    ( UpdateTestRunFailure
    , validateToDoneUpdate
    , validateToRunningUpdate
    )
import Oracle.Validate.Types
    ( AValidationResult
    , Validate
    , Validated
    , runValidate
    )
import Submitting (Submission (..))
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ToJSON (..)
    )
import User.Agent.Types (TestRunMap (..), TestRunStatus (..))
import User.Types
    ( Phase (..)
    , TestRun (..)
    , TestRunRejection
    , TestRunState (..)
    , URL (..)
    )
import Validation (Validation)

agentCmd
    :: Monad m
    => MPFS m
    -> Validation m
    -> Submission m
    -> Owner
    -> TokenId
    -> Owner
    -> AgentCommand NotReady a
    -> m a
agentCmd mpfs validation submit walletOwner tokenId agentId cmdNotReady = do
    mCmdReady <- resolveOldState mpfs tokenId cmdNotReady
    case mCmdReady of
        Nothing -> error "No previous state found for the command"
        Just cmdReady ->
            agentCmdCore
                mpfs
                validation
                submit
                walletOwner
                tokenId
                agentId
                cmdReady

withExpectedState
    :: (FromJSON Maybe (TestRunState phase), Monad m)
    => MPFS m
    -> TokenId
    -> TestRun
    -> (TestRunState phase -> m result)
    -> m (Maybe result)
withExpectedState mpfs tokenId testRun cont = do
    facts <- mpfsGetTokenFacts mpfs tokenId
    jsonKeyValue <- toJSON testRun
    let
        hasKey (JSObject obj) =
            any
                (\(k, value) -> k == "key" && value == jsonKeyValue)
                obj
        hasKey _ = False
    case facts of
        JSArray objects -> do
            case filter hasKey objects of
                [JSObject object] -> do
                    let value = find (\(k, _) -> k == "value") object
                    case value >>= fromJSON . snd of
                        Nothing -> pure Nothing
                        Just x -> Just <$> cont x
                _ -> pure Nothing
        _ -> pure Nothing

withResolvedTestRun
    :: Monad m
    => MPFS m
    -> TokenId
    -> TestRunId
    -> (TestRun -> m (Maybe a))
    -> m (Maybe a)
withResolvedTestRun mpfs tk (TestRunId testRunId) cont = do
    facts <- mpfsGetTokenFacts mpfs tk
    let testRuns = parseFacts facts
        finder :: Fact TestRun JSValue -> Bool
        finder (Fact key _) = case keyHash key of
            Nothing -> False
            Just keyId -> keyId == testRunId
    let mtr = factKey <$> find finder testRuns
    case mtr of
        Nothing -> pure Nothing
        Just testRun -> cont testRun

resolveOldState
    :: Monad m
    => MPFS m
    -> TokenId
    -> AgentCommand NotReady result
    -> m (Maybe (AgentCommand Ready result))
resolveOldState mpfs tokenId cmd = case cmd of
    Accept key () ->
        withResolvedTestRun mpfs tokenId key $ \testRun ->
            withExpectedState mpfs tokenId testRun $ pure . Accept testRun
    Reject key () reason ->
        withResolvedTestRun mpfs tokenId key $ \testRun ->
            withExpectedState mpfs tokenId testRun
                $ \pending -> pure $ Reject testRun pending reason
    Report key () duration url ->
        withResolvedTestRun mpfs tokenId key $ \testRun ->
            withExpectedState mpfs tokenId testRun $ \runningState ->
                pure $ Report testRun runningState duration url
    Query -> pure $ Just Query

data IsReady = NotReady | Ready
    deriving (Show, Eq)

type family IfReady a b where
    IfReady NotReady _ = ()
    IfReady Ready b = b

data Role = Internal | External
    deriving (Show, Eq)

newtype TestRunId = TestRunId
    { unTestRunId :: String
    }
    deriving (Show, Eq)

type family ResolveId phase where
    ResolveId NotReady = TestRunId
    ResolveId Ready = TestRun

data AgentCommand (phase :: IsReady) result where
    Accept
        :: ResolveId phase
        -> IfReady phase (TestRunState PendingT)
        -> AgentCommand
            phase
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState RunningT))
            )
    Reject
        :: ResolveId phase
        -> IfReady phase (TestRunState PendingT)
        -> [TestRunRejection]
        -> AgentCommand
            phase
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState DoneT))
            )
    Report
        :: ResolveId phase
        -> IfReady phase (TestRunState RunningT)
        -> Duration
        -> URL
        -> AgentCommand
            phase
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState DoneT))
            )
    Query :: AgentCommand phase TestRunMap

deriving instance Show (AgentCommand NotReady result)
deriving instance Eq (AgentCommand NotReady result)
deriving instance Show (AgentCommand Ready result)
deriving instance Eq (AgentCommand Ready result)

agentCmdCore
    :: Monad m
    => MPFS m
    -> Validation m
    -> Submission m
    -> Owner
    -> TokenId
    -> Owner
    -> AgentCommand Ready result
    -> m result
agentCmdCore mpfs validation submit walletOwner tokenId agentId cmd = case cmd of
    Accept key pending ->
        acceptCommand
            mpfs
            validation
            submit
            walletOwner
            tokenId
            agentId
            key
            pending
    Reject key pending reason ->
        rejectCommand
            mpfs
            validation
            submit
            walletOwner
            tokenId
            agentId
            key
            pending
            reason
    Report key running duration url ->
        reportCommand
            mpfs
            validation
            submit
            walletOwner
            tokenId
            agentId
            key
            running
            duration
            url
    Query -> queryCommand mpfs tokenId

queryCommand :: Monad m => MPFS m -> TokenId -> m TestRunMap
queryCommand mpfs tokenId = do
    facts <- mpfsGetTokenFacts mpfs tokenId
    let testRunsPending = parseFacts facts
        testRunsRunning = parseFacts facts
        testRunsDone = parseFacts facts
    pure
        $ TestRunMap
            { pending = testRunsPending <&> StatusPending
            , running = testRunsRunning <&> StatusRunning
            , done = testRunsDone <&> StatusDone
            }

signAndSubmitAnUpdate
    :: (ToJSON m key, ToJSON m old, ToJSON m new, Monad m)
    => MPFS m
    -> Validation m
    -> Submission m
    -> Owner
    -> ( Owner
         -> Validation m
         -> Owner
         -> Change key (OpU old new)
         -> Validate UpdateTestRunFailure m Validated
       )
    -> TokenId
    -> Owner
    -> key
    -> old
    -> new
    -> m (AValidationResult UpdateTestRunFailure (WithTxHash new))
signAndSubmitAnUpdate
    mpfs
    validation
    (Submission submit)
    walletOwner
    validate
    tokenId
    agentId
    testRun
    oldState
    newState = runValidate $ do
        void
            $ validate
                agentId
                validation
                walletOwner
            $ Change (Key testRun)
            $ Update oldState newState
        wtx <- lift $ submit $ \address -> do
            key <- toJSON testRun
            oldValue <- toJSON oldState
            newValue <- toJSON newState
            mpfsRequestUpdate mpfs address tokenId
                $ RequestUpdateBody{key, oldValue, newValue}
        pure $ wtx $> newState

reportCommand
    :: Monad m
    => MPFS m
    -> Validation m
    -> Submission m
    -> Owner
    -> TokenId
    -> Owner
    -> TestRun
    -> TestRunState RunningT
    -> Duration
    -> URL
    -> m
        ( AValidationResult
            UpdateTestRunFailure
            (WithTxHash (TestRunState DoneT))
        )
reportCommand mpfs validation submit walletOwner tokenId agentId testRun oldState duration url =
    signAndSubmitAnUpdate
        mpfs
        validation
        submit
        walletOwner
        validateToDoneUpdate
        tokenId
        agentId
        testRun
        oldState
        $ Finished oldState duration url

rejectCommand
    :: Monad m
    => MPFS m
    -> Validation m
    -> Submission m
    -> Owner
    -> TokenId
    -> Owner
    -> TestRun
    -> TestRunState PendingT
    -> [TestRunRejection]
    -> m
        ( AValidationResult
            UpdateTestRunFailure
            (WithTxHash (TestRunState DoneT))
        )
rejectCommand mpfs validation submit walletOwner tokenId agentId testRun testRunState reason =
    signAndSubmitAnUpdate
        mpfs
        validation
        submit
        walletOwner
        validateToDoneUpdate
        tokenId
        agentId
        testRun
        testRunState
        $ Rejected testRunState reason

acceptCommand
    :: Monad m
    => MPFS m
    -> Validation m
    -> Submission m
    -> Owner
    -> TokenId
    -> Owner
    -> TestRun
    -> TestRunState PendingT
    -> m
        ( AValidationResult
            UpdateTestRunFailure
            (WithTxHash (TestRunState RunningT))
        )
acceptCommand mpfs validation submit walletOwner tokenId agentId testRun testRunState =
    signAndSubmitAnUpdate
        mpfs
        validation
        submit
        walletOwner
        validateToRunningUpdate
        tokenId
        agentId
        testRun
        testRunState
        $ Accepted testRunState

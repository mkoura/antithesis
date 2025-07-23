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
import Core.Types.Wallet (Wallet (..))
import Data.Functor (($>), (<&>))
import Data.List (find)
import MPFS.API
    ( RequestUpdateBody (..)
    , getTokenFacts
    , requestUpdate
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
import Servant.Client (ClientM)
import Submitting (Submitting, signAndSubmit)
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
import Validation (Validation, mkValidation)

agentCmd
    :: Submitting
    -> Wallet
    -> TokenId
    -> Owner
    -> AgentCommand NotReady a
    -> ClientM a
agentCmd sbmt wallet tokenId agentId cmdNotReady = do
    mCmdReady <- resolveOldState tokenId cmdNotReady
    case mCmdReady of
        Nothing -> error "No previous state found for the command"
        Just cmdReady -> agentCmdCore sbmt wallet tokenId agentId cmdReady

withExpectedState
    :: FromJSON Maybe (TestRunState phase)
    => TokenId
    -> TestRun
    -> (TestRunState phase -> ClientM result)
    -> ClientM (Maybe result)
withExpectedState tokenId testRun cont = do
    facts <- getTokenFacts tokenId
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
    :: TokenId
    -> TestRunId
    -> (TestRun -> ClientM (Maybe a))
    -> ClientM (Maybe a)
withResolvedTestRun tk (TestRunId testRunId) cont = do
    facts <- getTokenFacts tk
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
    :: TokenId
    -> AgentCommand NotReady result
    -> ClientM (Maybe (AgentCommand Ready result))
resolveOldState tokenId cmd = case cmd of
    Accept key () ->
        withResolvedTestRun tokenId key $ \testRun ->
            withExpectedState tokenId testRun $ pure . Accept testRun
    Reject key () reason ->
        withResolvedTestRun tokenId key $ \testRun ->
            withExpectedState tokenId testRun
                $ \pending -> pure $ Reject testRun pending reason
    Report key () duration url ->
        withResolvedTestRun tokenId key $ \testRun ->
            withExpectedState tokenId testRun $ \runningState ->
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
    :: Submitting
    -> Wallet
    -> TokenId
    -> Owner
    -> AgentCommand Ready result
    -> ClientM result
agentCmdCore sbmt wallet tokenId agentId cmd = case cmd of
    Accept key pending ->
        acceptCommand sbmt wallet tokenId agentId key pending
    Reject key pending reason ->
        rejectCommand sbmt wallet tokenId agentId key pending reason
    Report key running duration url ->
        reportCommand sbmt wallet tokenId agentId key running duration url
    Query -> queryCommand tokenId

queryCommand :: TokenId -> ClientM TestRunMap
queryCommand tokenId = do
    facts <- getTokenFacts tokenId
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
    :: (ToJSON ClientM key, ToJSON ClientM old, ToJSON ClientM new)
    => Submitting
    -> Wallet
    -> ( Owner
         -> Validation ClientM
         -> Owner
         -> Change key (OpU old new)
         -> Validate UpdateTestRunFailure ClientM Validated
       )
    -> TokenId
    -> Owner
    -> key
    -> old
    -> new
    -> ClientM (AValidationResult UpdateTestRunFailure (WithTxHash new))
signAndSubmitAnUpdate
    sbmt
    wallet
    validate
    tokenId
    agentId
    testRun
    oldState
    newState = runValidate $ do
        void
            $ validate
                agentId
                (mkValidation tokenId)
                (owner wallet)
            $ Change (Key testRun)
            $ Update oldState newState
        wtx <- lift $ signAndSubmit sbmt wallet $ \address -> do
            key <- toJSON testRun
            oldValue <- toJSON oldState
            newValue <- toJSON newState
            requestUpdate address tokenId
                $ RequestUpdateBody{key, oldValue, newValue}
        pure $ wtx $> newState

reportCommand
    :: Submitting
    -> Wallet
    -> TokenId
    -> Owner
    -> TestRun
    -> TestRunState RunningT
    -> Duration
    -> URL
    -> ClientM
        ( AValidationResult
            UpdateTestRunFailure
            (WithTxHash (TestRunState DoneT))
        )
reportCommand sbmt wallet tokenId agentId testRun oldState duration url =
    signAndSubmitAnUpdate
        sbmt
        wallet
        validateToDoneUpdate
        tokenId
        agentId
        testRun
        oldState
        $ Finished oldState duration url

rejectCommand
    :: Submitting
    -> Wallet
    -> TokenId
    -> Owner
    -> TestRun
    -> TestRunState PendingT
    -> [TestRunRejection]
    -> ClientM
        ( AValidationResult
            UpdateTestRunFailure
            (WithTxHash (TestRunState DoneT))
        )
rejectCommand sbmt wallet tokenId agentId testRun testRunState reason =
    signAndSubmitAnUpdate
        sbmt
        wallet
        validateToDoneUpdate
        tokenId
        agentId
        testRun
        testRunState
        $ Rejected testRunState reason

acceptCommand
    :: Submitting
    -> Wallet
    -> TokenId
    -> Owner
    -> TestRun
    -> TestRunState PendingT
    -> ClientM
        ( AValidationResult
            UpdateTestRunFailure
            (WithTxHash (TestRunState RunningT))
        )
acceptCommand sbmt wallet tokenId agentId testRun testRunState =
    signAndSubmitAnUpdate
        sbmt
        wallet
        validateToRunningUpdate
        tokenId
        agentId
        testRun
        testRunState
        $ Accepted testRunState

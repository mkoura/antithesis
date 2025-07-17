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

import Core.Types.Basic (Duration, TokenId)
import Core.Types.Fact (Fact (..), keyHash, parseFacts)
import Core.Types.Tx (WithTxHash (..))
import Core.Types.Wallet (Wallet)
import Data.Functor ((<&>))
import Data.List (find)
import MPFS.API
    ( RequestUpdateBody (..)
    , getTokenFacts
    , requestUpdate
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

agentCmd
    :: Submitting
    -> Wallet
    -> TokenId
    -> AgentCommand NotReady a
    -> ClientM a
agentCmd sbmt wallet tokenId cmdNotReady = do
    mCmdReady <- resolveOldState tokenId cmdNotReady
    case mCmdReady of
        Nothing -> error "No previous state found for the command"
        Just cmdReady ->
            agentCmdCore sbmt wallet tokenId cmdReady

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
        -> AgentCommand phase (WithTxHash (TestRunState RunningT))
    Reject
        :: ResolveId phase
        -> IfReady phase (TestRunState PendingT)
        -> [TestRunRejection]
        -> AgentCommand phase (WithTxHash (TestRunState DoneT))
    Report
        :: ResolveId phase
        -> IfReady phase (TestRunState RunningT)
        -> Duration
        -> URL
        -> AgentCommand phase (WithTxHash (TestRunState DoneT))
    Query :: AgentCommand phase TestRunMap

deriving instance Show (AgentCommand NotReady result)
deriving instance Eq (AgentCommand NotReady result)
deriving instance Show (AgentCommand Ready result)
deriving instance Eq (AgentCommand Ready result)

agentCmdCore
    :: Submitting
    -> Wallet
    -> TokenId
    -> AgentCommand Ready result
    -> ClientM result
agentCmdCore sbmt wallet tokenId cmd = case cmd of
    Accept key pending -> acceptCommand sbmt wallet tokenId key pending
    Reject key pending reason ->
        rejectCommand sbmt wallet tokenId key pending reason
    Report key running duration url ->
        reportCommand sbmt wallet tokenId key running duration url
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
    :: (ToJSON ClientM old, ToJSON ClientM new, ToJSON ClientM res)
    => Submitting
    -> Wallet
    -> TokenId
    -> old
    -> new
    -> res
    -> ClientM (WithTxHash res)
signAndSubmitAnUpdate sbmt wallet tokenId testRun oldState newState = do
    WithTxHash txHash _ <- signAndSubmit sbmt wallet $ \address -> do
        key <- toJSON testRun
        oldValue <- toJSON oldState
        newValue <- toJSON newState
        requestUpdate address tokenId
            $ RequestUpdateBody{key, oldValue, newValue}
    pure $ WithTxHash txHash $ Just newState

reportCommand
    :: Submitting
    -> Wallet
    -> TokenId
    -> TestRun
    -> TestRunState RunningT
    -> Duration
    -> URL
    -> ClientM (WithTxHash (TestRunState DoneT))
reportCommand sbmt wallet tokenId testRun testRunState duration url =
    signAndSubmitAnUpdate sbmt wallet tokenId testRun testRunState
        $ Finished testRunState duration url

rejectCommand
    :: Submitting
    -> Wallet
    -> TokenId
    -> TestRun
    -> TestRunState PendingT
    -> [TestRunRejection]
    -> ClientM (WithTxHash (TestRunState DoneT))
rejectCommand sbmt wallet tokenId testRun testRunState reason =
    signAndSubmitAnUpdate sbmt wallet tokenId testRun testRunState
        $ Rejected testRunState reason

acceptCommand
    :: Submitting
    -> Wallet
    -> TokenId
    -> TestRun
    -> TestRunState PendingT
    -> ClientM (WithTxHash (TestRunState RunningT))
acceptCommand sbmt wallet tokenId testRun testRunState =
    signAndSubmitAnUpdate sbmt wallet tokenId testRun testRunState
        $ Accepted testRunState

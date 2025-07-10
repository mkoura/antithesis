{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module User.Agent.Cli
    ( AgentCommand (..)
    , IsReady (..)
    , agentCmd
    )
where

import Control.Monad (forM)
import Core.Types
    ( Duration
    , TokenId
    , Wallet
    , WithTxHash (..)
    , parseFacts
    )
import Data.List (find)
import MPFS.API
    ( RequestInsertBody (..)
    , RequestUpdateBody (..)
    , getTokenFacts
    , requestInsert
    , requestUpdate
    )
import Servant.Client (ClientM)
import Submitting (Submitting, signAndSubmit)
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ToJSON (..)
    )
import User.Agent.Validation.Config (AgentValidationConfig)
import User.Agent.Validation.Request (validateRequest)
import User.Types
    ( AgentValidation (..)
    , Phase (..)
    , TestRun (..)
    , TestRunRejection
    , TestRunState (..)
    , URL (..)
    )
import Validation (Validation)

agentCmd
    :: Submitting
    -> AgentValidationConfig
    -> Validation ClientM
    -> Wallet
    -> TokenId
    -> AgentCommand NotReady a
    -> ClientM a
agentCmd sbmt cfg validation wallet tokenId cmdNotReady = do
    mCmdReady <- resolveOldState tokenId cmdNotReady
    case mCmdReady of
        Nothing -> error "No previous state found for the command"
        Just cmdReady ->
            agentCmdCore sbmt cfg validation wallet tokenId cmdReady

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

resolveOldState
    :: TokenId
    -> AgentCommand NotReady result
    -> ClientM (Maybe (AgentCommand Ready result))
resolveOldState tokenId cmd = case cmd of
    Create key duration -> pure $ Just $ Create key duration
    Accept key () -> withExpectedState tokenId key $ pure . Accept key
    Reject key () reason ->
        withExpectedState tokenId key
            $ \pending -> pure $ Reject key pending reason
    Report key () duration url ->
        withExpectedState tokenId key $ \runningState ->
            pure $ Report key runningState duration url
    ValidateRequests -> pure $ Just ValidateRequests

data IsReady = NotReady | Ready
    deriving (Show, Eq)

type family IfReady a b where
    IfReady NotReady _ = ()
    IfReady Ready b = b

data Role = Internal | External
    deriving (Show, Eq)

data AgentCommand (phase :: IsReady) result where
    Create
        :: TestRun
        -> Duration
        -> AgentCommand phase (WithTxHash (TestRunState PendingT))
    Accept
        :: TestRun
        -> IfReady phase (TestRunState PendingT)
        -> AgentCommand phase (WithTxHash (TestRunState RunningT))
    Reject
        :: TestRun
        -> IfReady phase (TestRunState PendingT)
        -> [TestRunRejection]
        -> AgentCommand phase (WithTxHash (TestRunState DoneT))
    Report
        :: TestRun
        -> IfReady phase (TestRunState RunningT)
        -> Duration
        -> URL
        -> AgentCommand phase (WithTxHash (TestRunState DoneT))
    ValidateRequests :: AgentCommand phase [AgentValidation]

deriving instance Show (AgentCommand NotReady result)
deriving instance Eq (AgentCommand NotReady result)
deriving instance Show (AgentCommand Ready result)
deriving instance Eq (AgentCommand Ready result)

agentCmdCore
    :: Submitting
    -> AgentValidationConfig
    -> Validation ClientM
    -> Wallet
    -> TokenId
    -> AgentCommand Ready result
    -> ClientM result
agentCmdCore sbmt cfg validation wallet tokenId cmd = case cmd of
    Create key duration -> do
        createCommand sbmt wallet tokenId key duration
    Accept key pending -> acceptCommand sbmt wallet tokenId key pending
    Reject key pending reason ->
        rejectCommand sbmt wallet tokenId key pending reason
    Report key running duration url ->
        reportCommand sbmt wallet tokenId key running duration url
    ValidateRequests -> do
        factsObject <- getTokenFacts tokenId
        let parsed :: Maybe [(TestRun, TestRunState PendingT)] = do
                facts <- fromJSON factsObject
                pure $ parseFacts facts
        case parsed of
            Nothing -> error "Failed to parse facts"
            Just testRuns -> forM testRuns $ \(testRun, state) -> do
                mReasons <- validateRequest cfg validation testRun state
                pure
                    $ AgentValidation
                        { testRun
                        , validation = mReasons
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

createCommand
    :: Submitting
    -> Wallet
    -> TokenId
    -> TestRun
    -> Duration
    -> ClientM (WithTxHash (TestRunState PendingT))
createCommand sbmt wallet tokenId testRun duration = do
    let newState = Pending duration
    WithTxHash txHash _ <- signAndSubmit sbmt wallet $ \address -> do
        key <- toJSON testRun
        value <- toJSON newState
        requestInsert address tokenId
            $ RequestInsertBody{key, value}
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

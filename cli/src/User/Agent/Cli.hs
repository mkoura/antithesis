module User.Agent.Cli
    ( AgentCommand (..)
    , agentCmd
    )
where

import Core.Types (TokenId, Wallet, WithTxHash (..))
import MPFS.API
    ( RequestInsertBody (..)
    , RequestUpdateBody (..)
    , requestInsert
    , requestUpdate
    )
import Servant.Client (ClientM)
import Submitting (submitting)
import Text.JSON.Canonical (JSValue, ToJSON (..))
import User.Types
    ( Duration
    , Phase (..)
    , Reason
    , TestRun (..)
    , TestRunState (..)
    , URL (..)
    )

data AgentCommand
    = forall result.
        ToJSON ClientM result =>
      AgentCommand (AgentCommandCore result)

deriving instance Show AgentCommand

instance Eq AgentCommand where
    AgentCommand cmd1 == AgentCommand cmd2 = show cmd1 == show cmd2

agentCmd :: Wallet -> TokenId -> AgentCommand -> ClientM JSValue
agentCmd wallet tokenId (AgentCommand cmd) = do
    agentCmdCore wallet tokenId toJSON cmd

data AgentCommandCore result where
    Create
        :: TestRun
        -> Duration
        -> AgentCommandCore (TestRunState PendingT)
    Accept
        :: TestRun
        -> TestRunState PendingT
        -> AgentCommandCore (TestRunState RunningT)
    Reject
        :: TestRun
        -> TestRunState PendingT
        -> [Reason]
        -> AgentCommandCore (TestRunState DoneT)
    Report
        :: TestRun
        -> TestRunState RunningT
        -> Duration
        -> URL
        -> AgentCommandCore (TestRunState DoneT)
    List :: AgentCommandCore [TestRun]

deriving instance Show (AgentCommandCore result)
deriving instance Eq (AgentCommandCore result)

agentCmdCore
    :: Wallet
    -> TokenId
    -> (result -> ClientM a)
    -> AgentCommandCore result
    -> ClientM a
agentCmdCore wallet tokenId cont cmd = case cmd of
    Create testRun duration -> do
        createCommand wallet tokenId testRun duration >>= cont
    Accept key pending -> acceptCommand wallet tokenId key pending >>= cont
    Reject key pending reason ->
        rejectCommand wallet tokenId key pending reason >>= cont
    Report key running duration url ->
        reportCommand wallet tokenId key running duration url >>= cont
    List -> listTestRuns wallet tokenId >>= cont

listTestRuns :: Wallet -> TokenId -> ClientM [TestRun]
listTestRuns _wallet _tokenId = pure []

signAndSubmitAnUpdate
    :: (ToJSON ClientM old, ToJSON ClientM new, ToJSON ClientM res)
    => Wallet
    -> TokenId
    -> old
    -> new
    -> res
    -> ClientM res
signAndSubmitAnUpdate wallet tokenId testRun oldState newState = do
    WithTxHash _tx _ <- submitting wallet $ \address -> do
        key <- toJSON testRun
        oldValue <- toJSON oldState
        newValue <- toJSON newState
        requestUpdate address tokenId
            $ RequestUpdateBody{key, oldValue, newValue}
    pure newState

createCommand
    :: Wallet
    -> TokenId
    -> TestRun
    -> Duration
    -> ClientM (TestRunState PendingT)
createCommand wallet tokenId testRun duration = do
    let newState = Pending duration
    WithTxHash _tx _ <- submitting wallet $ \address -> do
        key <- toJSON testRun
        value <- toJSON newState
        requestInsert address tokenId
            $ RequestInsertBody{key, value}
    pure newState

reportCommand
    :: Wallet
    -> TokenId
    -> TestRun
    -> TestRunState RunningT
    -> Duration
    -> URL
    -> ClientM (TestRunState DoneT)
reportCommand wallet tokenId testRun testRunState duration url =
    signAndSubmitAnUpdate wallet tokenId testRun testRunState
        $ Finished testRunState duration url

rejectCommand
    :: Wallet
    -> TokenId
    -> TestRun
    -> TestRunState PendingT
    -> [Reason]
    -> ClientM (TestRunState DoneT)
rejectCommand wallet tokenId testRun testRunState reason =
    signAndSubmitAnUpdate wallet tokenId testRun testRunState
        $ Rejected testRunState reason

acceptCommand
    :: Wallet
    -> TokenId
    -> TestRun
    -> TestRunState PendingT
    -> ClientM (TestRunState RunningT)
acceptCommand wallet tokenId testRun testRunState =
    signAndSubmitAnUpdate wallet tokenId testRun testRunState
        $ Accepted testRunState

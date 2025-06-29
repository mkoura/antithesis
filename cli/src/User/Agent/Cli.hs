{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module User.Agent.Cli
    ( AgentCommand (..)
    , agentCmd
    , accept
    , reject
    , report
    , create
    )
where

import Core.Types (TokenId, Wallet, WithTxHash (..))
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import MPFS.API
    ( RequestInsertBody (..)
    , RequestUpdateBody (..)
    , getTokenFacts
    , requestInsert
    , requestUpdate
    )
import Servant.Client (ClientM)
import Submitting (submitting)
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ToJSON (..)
    , renderCanonicalJSON
    , toJSString
    )
import User.Types
    ( Duration
    , Phase (..)
    , Reason
    , TestRun (..)
    , TestRunState (..)
    , URL (..)
    )

accept :: TestRun -> AgentCommand
accept key = AgentCommand $ Accept key ()

reject :: TestRun -> [Reason] -> AgentCommand
reject key reason = AgentCommand $ Reject key () reason

report :: TestRun -> Duration -> URL -> AgentCommand
report key duration url = AgentCommand $ Report key () duration url

create :: TestRun -> Duration -> AgentCommand
create key duration = AgentCommand $ Create key duration

data AgentCommand
    = forall result.
        ToJSON ClientM result =>
      AgentCommand (AgentCommandCore result NotReady)

deriving instance Show AgentCommand
instance Eq AgentCommand where
    AgentCommand c1@Create{} == AgentCommand c2@Create{} = c1 == c2
    AgentCommand c1@Accept{} == AgentCommand c2@Accept{} = c1 == c2
    AgentCommand c1@Reject{} == AgentCommand c2@Reject{} = c1 == c2
    AgentCommand c1@Report{} == AgentCommand c2@Report{} = c1 == c2
    _ == _ = False

agentCmd :: Wallet -> TokenId -> AgentCommand -> ClientM JSValue
agentCmd wallet tokenId (AgentCommand cmdNotReady) = do
    mCmdReady <- resolveOldState tokenId cmdNotReady
    case mCmdReady of
        Nothing -> pure JSNull
        Just cmdReady -> agentCmdCore wallet tokenId toJSON cmdReady

withExpectedState
    :: FromJSON Maybe (TestRunState phase)
    => TokenId
    -> TestRun
    -> (TestRunState phase -> ClientM result)
    -> ClientM (Maybe result)
withExpectedState tokenId testRun cont = do
    facts <- getTokenFacts tokenId
    jsonKeyValue <- toJSON testRun
    let jsonKey =
            toJSString
                $ T.unpack
                $ decodeUtf8
                $ BL.toStrict
                $ renderCanonicalJSON jsonKeyValue
    case facts of
        JSObject mapping -> do
            case lookup jsonKey mapping of
                Just value -> do
                    case fromJSON value of
                        Nothing -> pure Nothing
                        Just x -> Just <$> cont x
                Nothing -> pure Nothing
        _ -> pure Nothing

resolveOldState
    :: TokenId
    -> AgentCommandCore result NotReady
    -> ClientM (Maybe (AgentCommandCore result Ready))
resolveOldState tokenId cmd = case cmd of
    Create key duration -> pure $ Just $ Create key duration
    Accept key () -> withExpectedState tokenId key $ pure . Accept key
    Reject key () reason ->
        withExpectedState tokenId key
            $ \pending -> pure $ Reject key pending reason
    Report key () duration url ->
        withExpectedState tokenId key $ \runningState ->
            pure $ Report key runningState duration url

data IsReady = NotReady | Ready
    deriving (Show, Eq)

type family IfReady a b where
    IfReady NotReady _ = ()
    IfReady Ready b = b

data AgentCommandCore result (phase :: IsReady) where
    Create
        :: TestRun
        -> Duration
        -> AgentCommandCore (TestRunState PendingT) phase
    Accept
        :: TestRun
        -> IfReady phase (TestRunState PendingT)
        -> AgentCommandCore (TestRunState RunningT) phase
    Reject
        :: TestRun
        -> IfReady phase (TestRunState PendingT)
        -> [Reason]
        -> AgentCommandCore (TestRunState DoneT) phase
    Report
        :: TestRun
        -> IfReady phase (TestRunState RunningT)
        -> Duration
        -> URL
        -> AgentCommandCore (TestRunState DoneT) phase

deriving instance Show (AgentCommandCore result NotReady)
deriving instance Eq (AgentCommandCore result NotReady)
deriving instance Show (AgentCommandCore result Ready)
deriving instance Eq (AgentCommandCore result Ready)

agentCmdCore
    :: Wallet
    -> TokenId
    -> (WithTxHash result -> ClientM a)
    -> AgentCommandCore result Ready
    -> ClientM a
agentCmdCore wallet tokenId cont cmd = case cmd of
    Create key duration -> do
        createCommand wallet tokenId key duration >>= cont
    Accept key pending -> acceptCommand wallet tokenId key pending >>= cont
    Reject key pending reason ->
        rejectCommand wallet tokenId key pending reason >>= cont
    Report key running duration url ->
        reportCommand wallet tokenId key running duration url >>= cont

signAndSubmitAnUpdate
    :: (ToJSON ClientM old, ToJSON ClientM new, ToJSON ClientM res)
    => Wallet
    -> TokenId
    -> old
    -> new
    -> res
    -> ClientM (WithTxHash res)
signAndSubmitAnUpdate wallet tokenId testRun oldState newState = do
    WithTxHash txHash _ <- submitting wallet $ \address -> do
        key <- toJSON testRun
        oldValue <- toJSON oldState
        newValue <- toJSON newState
        requestUpdate address tokenId
            $ RequestUpdateBody{key, oldValue, newValue}
    pure $ WithTxHash txHash $ Just newState

createCommand
    :: Wallet
    -> TokenId
    -> TestRun
    -> Duration
    -> ClientM (WithTxHash (TestRunState PendingT))
createCommand wallet tokenId testRun duration = do
    let newState = Pending duration
    WithTxHash txHash _ <- submitting wallet $ \address -> do
        key <- toJSON testRun
        value <- toJSON newState
        requestInsert address tokenId
            $ RequestInsertBody{key, value}
    pure $ WithTxHash txHash $ Just newState

reportCommand
    :: Wallet
    -> TokenId
    -> TestRun
    -> TestRunState RunningT
    -> Duration
    -> URL
    -> ClientM (WithTxHash (TestRunState DoneT))
reportCommand wallet tokenId testRun testRunState duration url =
    signAndSubmitAnUpdate wallet tokenId testRun testRunState
        $ Finished testRunState duration url

rejectCommand
    :: Wallet
    -> TokenId
    -> TestRun
    -> TestRunState PendingT
    -> [Reason]
    -> ClientM (WithTxHash (TestRunState DoneT))
rejectCommand wallet tokenId testRun testRunState reason =
    signAndSubmitAnUpdate wallet tokenId testRun testRunState
        $ Rejected testRunState reason

acceptCommand
    :: Wallet
    -> TokenId
    -> TestRun
    -> TestRunState PendingT
    -> ClientM (WithTxHash (TestRunState RunningT))
acceptCommand wallet tokenId testRun testRunState =
    signAndSubmitAnUpdate wallet tokenId testRun testRunState
        $ Accepted testRunState

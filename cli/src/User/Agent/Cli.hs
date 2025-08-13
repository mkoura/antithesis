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
import Core.Context
    ( WithContext
    , askAgentPKH
    , askMpfs
    , askSubmit
    , askValidation
    , withMPFS
    )
import Core.Types.Basic
    ( Duration
    , Owner
    , Platform
    , Repository
    , TokenId
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..), keyHash, parseFacts)
import Core.Types.Operation (Op (..), Operation (..))
import Core.Types.Tx (WithTxHash (..))
import Data.Functor (($>), (<&>))
import Data.List (find)
import MPFS.API
    ( MPFS (..)
    , RequestDeleteBody (..)
    , RequestInsertBody (..)
    , RequestUpdateBody (..)
    )
import Oracle.Validate.Requests.ManageWhiteList
    ( UpdateWhiteListFailure
    , validateAddWhiteListed
    , validateRemoveWhiteListed
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
import User.Agent.Types
    ( TestRunMap (..)
    , TestRunStatus (..)
    , WhiteListKey (..)
    )
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
    => Owner
    -> AgentCommand NotReady a
    -> WithContext m a
agentCmd agentId cmdNotReady = do
    mCmdReady <- resolveOldState cmdNotReady
    case mCmdReady of
        Nothing -> error "No previous state found for the command"
        Just cmdReady -> agentCmdCore agentId cmdReady

withExpectedState
    :: (FromJSON Maybe (TestRunState phase), Monad m)
    => TokenId
    -> TestRun
    -> (TestRunState phase -> WithContext m result)
    -> WithContext m (Maybe result)
withExpectedState tokenId testRun cont = do
    facts <- withMPFS $ \mpfs -> mpfsGetTokenFacts mpfs tokenId
    jsonKeyValue <- toJSON testRun
    let
        hasKey (JSObject obj) =
            any (\(k, value) -> k == "key" && value == jsonKeyValue) obj
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
    => TokenId
    -> TestRunId
    -> (TestRun -> WithContext m (Maybe a))
    -> WithContext m (Maybe a)
withResolvedTestRun tk (TestRunId testRunId) cont = do
    facts <- withMPFS $ \mpfs -> mpfsGetTokenFacts mpfs tk
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
    => AgentCommand NotReady result
    -> WithContext m (Maybe (AgentCommand Ready result))
resolveOldState cmd = case cmd of
    Accept tokenId key () ->
        withResolvedTestRun tokenId key $ \testRun ->
            withExpectedState tokenId testRun $ pure . Accept tokenId testRun
    Reject tokenId key () reason ->
        withResolvedTestRun tokenId key $ \testRun ->
            withExpectedState tokenId testRun
                $ \pending -> pure $ Reject tokenId testRun pending reason
    Report tokenId key () duration url ->
        withResolvedTestRun tokenId key $ \testRun ->
            withExpectedState tokenId testRun $ \runningState ->
                pure $ Report tokenId testRun runningState duration url
    Query tokenId -> pure $ Just $ Query tokenId
    WhiteList tokenId platform repo ->
        pure $ Just $ WhiteList tokenId platform repo
    BlackList tokenId platform repo ->
        pure $ Just $ BlackList tokenId platform repo

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
        :: TokenId
        -> ResolveId phase
        -> IfReady phase (TestRunState PendingT)
        -> AgentCommand
            phase
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState RunningT))
            )
    Reject
        :: TokenId
        -> ResolveId phase
        -> IfReady phase (TestRunState PendingT)
        -> [TestRunRejection]
        -> AgentCommand
            phase
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState DoneT))
            )
    Report
        :: TokenId
        -> ResolveId phase
        -> IfReady phase (TestRunState RunningT)
        -> Duration
        -> URL
        -> AgentCommand
            phase
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState DoneT))
            )
    Query :: TokenId -> AgentCommand phase TestRunMap
    WhiteList
        :: TokenId
        -> Platform
        -> Repository
        -> AgentCommand
            phase
            (AValidationResult UpdateWhiteListFailure (WithTxHash ()))
    BlackList
        :: TokenId
        -> Platform
        -> Repository
        -> AgentCommand
            phase
            (AValidationResult UpdateWhiteListFailure (WithTxHash ()))

deriving instance Show (AgentCommand NotReady result)
deriving instance Eq (AgentCommand NotReady result)
deriving instance Show (AgentCommand Ready result)
deriving instance Eq (AgentCommand Ready result)

agentCmdCore
    :: Monad m
    => Owner
    -- ^ requester public key hash
    -> AgentCommand Ready result
    -> WithContext m result
agentCmdCore requester cmd = case cmd of
    Accept tokenId key pending ->
        acceptCommand tokenId requester key pending
    Reject tokenId key pending reason ->
        rejectCommand tokenId requester key pending reason
    Report tokenId key running duration url ->
        reportCommand tokenId requester key running duration url
    Query tokenId -> queryCommand tokenId
    WhiteList tokenId platform repo -> whiteList tokenId requester platform repo
    BlackList tokenId platform repo -> blackList tokenId requester platform repo

whiteList
    :: Monad m
    => TokenId
    -> Owner
    -> Platform
    -> Repository
    -> WithContext
        m
        ( AValidationResult
            UpdateWhiteListFailure
            (WithTxHash ())
        )
whiteList tokenId requester platform repo = do
    let key = WhiteListKey platform repo
        change =
            Change
                { key = Key key
                , operation = Insert ()
                }
    validation <- askValidation tokenId
    agentPKH <- askAgentPKH
    Submission submit <- askSubmit
    mpfs <- askMpfs
    lift $ runValidate $ do
        void $ validateAddWhiteListed validation requester agentPKH change
        wtx <- lift $ submit $ \address -> do
            jkey <- toJSON key
            mpfsRequestInsert mpfs address tokenId
                $ RequestInsertBody{key = jkey, value = JSNull}
        pure $ wtx $> ()

blackList
    :: Monad m
    => TokenId
    -> Owner
    -> Platform
    -> Repository
    -> WithContext
        m
        ( AValidationResult
            UpdateWhiteListFailure
            (WithTxHash ())
        )
blackList tokenId requester platform repo = do
    let key = WhiteListKey platform repo
        change = Change (Key key) (Delete ())
    validation <- askValidation tokenId
    agentPKH <- askAgentPKH
    Submission submit <- askSubmit
    mpfs <- askMpfs
    lift $ runValidate $ do
        void $ validateRemoveWhiteListed validation requester agentPKH change
        wtx <- lift $ submit $ \address -> do
            jkey <- toJSON key
            mpfsRequestDelete mpfs address tokenId
                $ RequestDeleteBody{key = jkey, value = JSNull}
        pure $ wtx $> ()

queryCommand :: Monad m => TokenId -> WithContext m TestRunMap
queryCommand tokenId = do
    facts <- withMPFS $ \mpfs -> mpfsGetTokenFacts mpfs tokenId
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
    => ( Validation m
         -> Owner
         -> Owner
         -> Change key (OpU old new)
         -> Validate UpdateTestRunFailure m Validated
       )
    -> TokenId
    -> Owner
    -> key
    -> old
    -> new
    -> WithContext
        m
        (AValidationResult UpdateTestRunFailure (WithTxHash new))
signAndSubmitAnUpdate validate tokenId requester testRun oldState newState = do
    validation <- askValidation tokenId
    agentPKH <- askAgentPKH
    Submission submit <- askSubmit
    mpfs <- askMpfs
    lift $ runValidate $ do
        void
            $ validate validation agentPKH requester
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
    => TokenId
    -> Owner
    -> TestRun
    -> TestRunState RunningT
    -> Duration
    -> URL
    -> WithContext
        m
        ( AValidationResult
            UpdateTestRunFailure
            (WithTxHash (TestRunState DoneT))
        )
reportCommand tokenId requester testRun oldState duration url =
    signAndSubmitAnUpdate
        validateToDoneUpdate
        tokenId
        requester
        testRun
        oldState
        $ Finished oldState duration url

rejectCommand
    :: Monad m
    => TokenId
    -> Owner
    -> TestRun
    -> TestRunState PendingT
    -> [TestRunRejection]
    -> WithContext
        m
        ( AValidationResult
            UpdateTestRunFailure
            (WithTxHash (TestRunState DoneT))
        )
rejectCommand tokenId requester testRun testRunState reason =
    signAndSubmitAnUpdate
        validateToDoneUpdate
        tokenId
        requester
        testRun
        testRunState
        $ Rejected testRunState reason

acceptCommand
    :: Monad m
    => TokenId
    -> Owner
    -> TestRun
    -> TestRunState PendingT
    -> WithContext
        m
        ( AValidationResult
            UpdateTestRunFailure
            (WithTxHash (TestRunState RunningT))
        )
acceptCommand tokenId requester testRun testRunState =
    signAndSubmitAnUpdate
        validateToRunningUpdate
        tokenId
        requester
        testRun
        testRunState
        $ Accepted testRunState

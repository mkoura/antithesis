{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module User.Agent.Cli
    ( AgentCommand (..)
    , TestRunId (..)
    , IsReady (..)
    , agentCmd
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Core.Context
    ( WithContext
    , askConfig
    , askMpfs
    , askSubmit
    , askValidation
    , withMPFS
    )
import Core.Types.Basic
    ( Directory
    , Duration
    , Owner
    , Platform
    , Repository
    , TokenId
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..), keyHash, parseFacts)
import Core.Types.Operation (Op (..), Operation (..))
import Core.Types.Tx (WithTxHash (..))
import Core.Types.Wallet (Wallet (..))
import Data.Functor (($>), (<&>))
import Data.List (find)
import MPFS.API
    ( MPFS (..)
    , RequestDeleteBody (..)
    , RequestInsertBody (..)
    , RequestUpdateBody (..)
    )
import Oracle.Config.Types (Config (..))
import Oracle.Validate.DownloadAssets
    ( DownloadAssetsFailure
    , validateDownloadAssets
    )
import Oracle.Validate.Requests.ManageWhiteList
    ( UpdateWhiteListFailure (..)
    , validateAddWhiteListed
    , validateRemoveWhiteListed
    )
import Oracle.Validate.Requests.TestRun.Update
    ( UpdateTestRunFailure (..)
    , validateToDoneUpdate
    , validateToRunningUpdate
    )
import Oracle.Validate.Types
    ( AValidationResult (..)
    , Validate
    , Validated
    , liftMaybe
    , runValidate
    )
import Submitting (Submission (..))
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ToJSON (..)
    )
import User.Agent.Types
    ( TestRunId (..)
    , TestRunMap (..)
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

updateTestRunState
    :: (Monad m, FromJSON Maybe (TestRunState phase))
    => TokenId
    -> TestRunId
    -> ( TestRun
         -> TestRunState phase
         -> WithContext m (AValidationResult UpdateTestRunFailure a)
       )
    -> WithContext m (AValidationResult UpdateTestRunFailure a)
updateTestRunState tokenId key f = do
    mResult <- withResolvedTestRun tokenId key $ \testRun ->
        withExpectedState tokenId testRun $ f testRun
    pure $ case mResult of
        Nothing -> ValidationFailure UpdateTestRunWrongPreviousState
        Just result -> result

agentCmd
    :: MonadIO m
    => AgentCommand NotReady a
    -> WithContext m a
agentCmd = \case
    Query tokenId -> queryCommand tokenId
    WhiteList tokenId wallet platform repo ->
        whiteList tokenId wallet platform repo
    BlackList tokenId wallet platform repo ->
        blackList tokenId wallet platform repo
    DownloadAssets tokenId key dir -> downloadAssets tokenId key dir
    Accept tokenId wallet key () -> do
        updateTestRunState tokenId key $ \testRun testRunState ->
            acceptCommand tokenId wallet testRun testRunState
    Reject tokenId wallet key () reason -> do
        updateTestRunState tokenId key $ \testRun testRunState ->
            rejectCommand tokenId wallet testRun testRunState reason
    Report tokenId wallet key () duration url -> do
        updateTestRunState tokenId key $ \testRun testRunState ->
            reportCommand tokenId wallet testRun testRunState duration url

data IsReady = NotReady | Ready
    deriving (Show, Eq)

type family IfReady a b where
    IfReady NotReady _ = ()
    IfReady Ready b = b

data Role = Internal | External
    deriving (Show, Eq)

type family ResolveId phase where
    ResolveId NotReady = TestRunId
    ResolveId Ready = TestRun

data AgentCommand (phase :: IsReady) result where
    Accept
        :: TokenId
        -> Wallet
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
        -> Wallet
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
        -> Wallet
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
        -> Wallet
        -> Platform
        -> Repository
        -> AgentCommand
            phase
            (AValidationResult UpdateWhiteListFailure (WithTxHash ()))
    BlackList
        :: TokenId
        -> Wallet
        -> Platform
        -> Repository
        -> AgentCommand
            phase
            (AValidationResult UpdateWhiteListFailure (WithTxHash ()))
    DownloadAssets
        :: TokenId
        -> TestRunId
        -> Directory
        -> AgentCommand
            phase
            (AValidationResult DownloadAssetsFailure ())

deriving instance Show (AgentCommand NotReady result)
deriving instance Eq (AgentCommand NotReady result)
deriving instance Show (AgentCommand Ready result)
deriving instance Eq (AgentCommand Ready result)

whiteList
    :: Monad m
    => TokenId
    -> Wallet
    -> Platform
    -> Repository
    -> WithContext
        m
        ( AValidationResult
            UpdateWhiteListFailure
            (WithTxHash ())
        )
whiteList tokenId wallet platform repo = do
    let key = WhiteListKey platform repo
        change =
            Change
                { key = Key key
                , operation = Insert ()
                }
        requester = owner wallet
    validation <- askValidation $ Just tokenId
    Submission submit <- ($ wallet) <$> askSubmit
    mpfs <- askMpfs
    mconfig <- askConfig tokenId
    lift $ runValidate $ do
        Config{configAgent} <- liftMaybe WhiteListConfigNotAvailable mconfig
        void $ validateAddWhiteListed validation requester configAgent change
        wtx <- lift $ submit $ \address -> do
            jkey <- toJSON key
            mpfsRequestInsert mpfs address tokenId
                $ RequestInsertBody{key = jkey, value = JSNull}
        pure $ wtx $> ()

blackList
    :: Monad m
    => TokenId
    -> Wallet
    -> Platform
    -> Repository
    -> WithContext
        m
        ( AValidationResult
            UpdateWhiteListFailure
            (WithTxHash ())
        )
blackList tokenId wallet platform repo = do
    let key = WhiteListKey platform repo
        change = Change (Key key) (Delete ())
        requester = owner wallet
    validation <- askValidation $ Just tokenId
    Submission submit <- ($ wallet) <$> askSubmit
    mpfs <- askMpfs
    mconfig <- askConfig tokenId
    lift $ runValidate $ do
        Config{configAgent} <- liftMaybe WhiteListConfigNotAvailable mconfig
        void
            $ validateRemoveWhiteListed validation requester configAgent change
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

downloadAssets
    :: MonadIO m
    => TokenId
    -> TestRunId
    -> Directory
    -> WithContext m (AValidationResult DownloadAssetsFailure ())
downloadAssets tokenId testRunId dir = do
    testmap <- queryCommand tokenId
    validation <- askValidation $ Just tokenId
    lift $ runValidate $ do
        void $ validateDownloadAssets validation testmap testRunId dir

signAndSubmitAnUpdate
    :: (ToJSON m key, ToJSON m old, ToJSON m new, Monad m)
    => ( Validation m
         -> Owner
         -> Owner
         -> Change key (OpU old new)
         -> Validate UpdateTestRunFailure m Validated
       )
    -> TokenId
    -> Wallet
    -> key
    -> old
    -> new
    -> WithContext
        m
        (AValidationResult UpdateTestRunFailure (WithTxHash new))
signAndSubmitAnUpdate validate tokenId wallet testRun oldState newState = do
    let requester = owner wallet
    validation <- askValidation $ Just tokenId
    mconfig <- askConfig tokenId
    Submission submit <- ($ wallet) <$> askSubmit
    mpfs <- askMpfs
    lift $ runValidate $ do
        Config{configAgent} <-
            liftMaybe UpdateTestRunConfigNotAvailable mconfig
        void
            $ validate validation configAgent requester
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
    -> Wallet
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
reportCommand tokenId wallet testRun oldState duration url =
    signAndSubmitAnUpdate
        validateToDoneUpdate
        tokenId
        wallet
        testRun
        oldState
        $ Finished oldState duration url

rejectCommand
    :: Monad m
    => TokenId
    -> Wallet
    -> TestRun
    -> TestRunState PendingT
    -> [TestRunRejection]
    -> WithContext
        m
        ( AValidationResult
            UpdateTestRunFailure
            (WithTxHash (TestRunState DoneT))
        )
rejectCommand tokenId wallet testRun testRunState reason =
    signAndSubmitAnUpdate
        validateToDoneUpdate
        tokenId
        wallet
        testRun
        testRunState
        $ Rejected testRunState reason

acceptCommand
    :: Monad m
    => TokenId
    -> Wallet
    -> TestRun
    -> TestRunState PendingT
    -> WithContext
        m
        ( AValidationResult
            UpdateTestRunFailure
            (WithTxHash (TestRunState RunningT))
        )
acceptCommand tokenId wallet testRun testRunState =
    signAndSubmitAnUpdate
        validateToRunningUpdate
        tokenId
        wallet
        testRun
        testRunState
        $ Accepted testRunState

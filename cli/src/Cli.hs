module Cli
    ( cmd
    , Command (..)
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Core.Context (askConfig, askMpfs, askValidation, withContext)
import Core.Types.Basic (RequestRefId, TokenId)
import Core.Types.MPFS (MPFSClient (..))
import Core.Types.Tx (TxHash, WithTxHash (..))
import Core.Types.Wallet (Wallet)
import Data.Functor.Identity (Identity (..))
import Lib.GitHub (getOAUth)
import Lib.JSON.Canonical.Extra
import MPFS.API
    ( MPFS (..)
    , getTokenFacts
    , mpfsClient
    , retractChange
    )
import Oracle.Cli (OracleCommand (..), oracleCmd)
import Oracle.Types
    ( RequestValidationFailure
    , Token (..)
    , TokenState (..)
    , fmapMToken
    )
import Oracle.Validate.Request (validateRequest)
import Oracle.Validate.Types
    ( AValidationResult
    , ValidationResult
    , liftMaybe
    , runValidate
    )
import Submitting (Submission (..))
import Text.JSON.Canonical (FromJSON (..), JSValue, ToJSON (..))
import User.Agent.Cli
    ( AgentCommand (..)
    , IsReady (NotReady)
    , agentCmd
    )
import User.Requester.Cli
    ( RequesterCommand
    , requesterCmd
    )
import Validation (mkValidation)
import Wallet.Cli (WalletCommand, walletCmd)

data Command a where
    RequesterCommand :: MPFSClient -> RequesterCommand a -> Command a
    OracleCommand :: MPFSClient -> OracleCommand a -> Command a
    AgentCommand :: MPFSClient -> AgentCommand NotReady a -> Command a
    RetractRequest
        :: MPFSClient -> Wallet -> RequestRefId -> Command TxHash
    GetFacts :: MPFSClient -> TokenId -> Command JSValue
    Wallet :: WalletCommand a -> Command a
    GetToken
        :: MPFSClient
        -> TokenId
        -> Command
            (AValidationResult TokenInfoFailure (Token WithValidation))

data SetupError = TokenNotSpecified
    deriving (Show, Eq)

cmd :: Command a -> IO a
cmd = \case
    RequesterCommand
        MPFSClient{runMPFS, submitTx}
        requesterCommand -> do
            auth <- getOAUth
            runMPFS
                $ withContext
                    mpfsClient
                    (mkValidation auth)
                    submitTx
                $ requesterCmd requesterCommand
    OracleCommand
        MPFSClient{runMPFS, submitTx}
        oracleCommand -> do
            auth <- getOAUth
            runMPFS
                $ withContext
                    mpfsClient
                    (mkValidation auth)
                    submitTx
                $ oracleCmd oracleCommand
    AgentCommand
        MPFSClient{runMPFS, submitTx}
        agentCommand -> do
            auth <- getOAUth
            runMPFS
                $ withContext
                    mpfsClient
                    (mkValidation auth)
                    submitTx
                $ agentCmd agentCommand
    RetractRequest
        MPFSClient{runMPFS, submitTx}
        wallet
        refId -> do
            let Submission submit = submitTx wallet
            runMPFS
                $ fmap txHash
                $ submit
                $ \address ->
                    retractChange address refId
    GetFacts MPFSClient{runMPFS} tokenId -> do
        runMPFS $ getTokenFacts tokenId
    Wallet walletCommand -> liftIO $ walletCmd walletCommand
    GetToken
        MPFSClient{runMPFS, submitTx}
        tk -> do
            auth <- getOAUth
            runMPFS
                $ withContext
                    mpfsClient
                    (mkValidation auth)
                    submitTx
                $ do
                    validation <- askValidation $ Just tk
                    mconfig <- askConfig tk
                    mpfs <- askMpfs
                    lift $ runValidate $ do
                        mpendings <- lift $ fromJSON <$> mpfsGetToken mpfs tk
                        token <- liftMaybe (TokenInfoTokenNotParsable tk) mpendings
                        let oracle = tokenOwner $ tokenState token
                            f (Identity req) = do
                                r <-
                                    runValidate
                                        $ validateRequest oracle mconfig validation req
                                pure $ WithValidation r req
                        lift $ fmapMToken f token

newtype TokenInfoFailure = TokenInfoTokenNotParsable TokenId
    deriving (Show, Eq)

instance Monad m => ToJSON m TokenInfoFailure where
    toJSON (TokenInfoTokenNotParsable tk) =
        object ["tokenNotParsable" .= tk]

data WithValidation x = WithValidation
    { validation :: ValidationResult RequestValidationFailure
    , request :: x
    }
    deriving (Show, Eq)

instance (Monad m, ToJSON m x) => ToJSON m (WithValidation x) where
    toJSON (WithValidation mvalidation request) =
        object
            [ "validation" .= mvalidation
            , "request" .= request
            ]

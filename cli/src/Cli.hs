module Cli
    ( cmd
    , Command (..)
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Core.Context (withContext)
import Core.Types.Basic (RequestRefId, TokenId)
import Core.Types.MPFS (MPFSClient (..))
import Core.Types.Tx (TxHash, WithTxHash (..))
import Core.Types.Wallet (Wallet)
import Lib.GitHub (getOAUth)
import MPFS.API
    ( getTokenFacts
    , mpfsClient
    , retractChange
    )
import Oracle.Cli (OracleCommand (..), oracleCmd)
import Submitting (Submission (..))
import Text.JSON.Canonical (JSValue)
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

module Cli
    ( cmd
    , Command (..)
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Core.Context (withContext)
import Core.Types.Basic (RequestRefId, TokenId)
import Core.Types.Tx (TxHash, WithTxHash (..))
import Core.Types.Wallet (Wallet)
import Lib.GitHub (getOAUth)
import MPFS.API (getTokenFacts, mpfsClient, retractChange)
import Oracle.Cli (OracleCommand (..), oracleCmd)
import Servant.Client (ClientM)
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
    RequesterCommand :: RequesterCommand a -> Command a
    OracleCommand :: OracleCommand a -> Command a
    AgentCommand :: AgentCommand NotReady a -> Command a
    RetractRequest :: Wallet -> RequestRefId -> Command TxHash
    GetFacts :: TokenId -> Command JSValue
    Wallet :: WalletCommand a -> Command a

deriving instance Show (Command a)
deriving instance Eq (Command a)

data SetupError = TokenNotSpecified
    deriving (Show, Eq)

cmd
    :: (Wallet -> Submission ClientM)
    -> Command a
    -> ClientM a
cmd
    submit =
        \case
            RequesterCommand requesterCommand -> do
                auth <- liftIO getOAUth
                withContext
                    mpfsClient
                    (mkValidation auth)
                    submit
                    $ requesterCmd requesterCommand
            OracleCommand oracleCommand -> do
                auth <- liftIO getOAUth
                withContext
                    mpfsClient
                    (mkValidation auth)
                    submit
                    $ oracleCmd
                        oracleCommand
            AgentCommand agentCommand -> do
                auth <- liftIO getOAUth
                withContext
                    mpfsClient
                    (mkValidation auth)
                    submit
                    $ agentCmd agentCommand
            RetractRequest wallet refId -> do
                let Submission submit' = submit wallet
                fmap txHash $ submit' $ \address ->
                    retractChange address refId
            GetFacts tokenId -> getTokenFacts tokenId
            Wallet walletCommand -> liftIO $ walletCmd walletCommand

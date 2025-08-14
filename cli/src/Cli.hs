module Cli
    ( cmd
    , Command (..)
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Core.Context (withContext)
import Core.Types.Basic (RequestRefId, TokenId)
import Core.Types.Tx (TxHash, WithTxHash (..))
import Core.Types.Wallet (Wallet (owner))
import Lib.GitHub (getOAUth)
import Lib.SSH.Private
    ( KeyAPI (..)
    , SSHKeySelector (SSHKeySelector)
    , SigningMap
    )
import MPFS.API (getTokenFacts, mpfsClient, retractChange)
import Oracle.Cli (OracleCommand (..), oracleCmd)
import Servant.Client (ClientM)
import Submitting (Submission (..))
import System.Environment (getEnv)
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
    RetractRequest :: RequestRefId -> Command TxHash
    GetFacts :: TokenId -> Command JSValue
    Wallet :: WalletCommand a -> Command a

deriving instance Show (Command a)
deriving instance Eq (Command a)

failNothing :: Applicative m => [Char] -> Maybe a -> m a
failNothing w Nothing = error w
failNothing _ (Just x) = pure x

failLeft :: Applicative m => (a -> String) -> Either a b -> m b
failLeft f (Left err) = error $ f err
failLeft _ (Right x) = pure x

data SetupError = TokenNotSpecified
    deriving (Show, Eq)

cmd
    :: (Wallet -> Submission ClientM)
    -> Either FilePath Wallet
    -> Maybe SigningMap
    -> Command a
    -> ClientM a
cmd
    submit
    mWallet
    mSigning =
        \case
            RequesterCommand requesterCommand -> do
                signing <- failNothing "No SSH file" mSigning
                sshKeySelector <- liftIO $ getEnv "ANTI_SSH_KEY_SELECTOR"
                auth <- liftIO getOAUth
                keyAPI <-
                    failNothing (sshKeySelector <> "not in the signing map")
                        $ signing
                        $ SSHKeySelector sshKeySelector
                wallet <- failLeft ("No wallet @ " <>) mWallet
                withContext
                    mpfsClient
                    (mkValidation auth)
                    (submit wallet)
                    $ requesterCmd (sign keyAPI) requesterCommand
            OracleCommand oracleCommand -> do
                wallet <- failLeft ("No wallet @ " <>) mWallet
                auth <- liftIO getOAUth
                withContext
                    mpfsClient
                    (mkValidation auth)
                    (submit wallet)
                    $ oracleCmd
                        oracleCommand
            AgentCommand agentCommand -> do
                auth <- liftIO getOAUth
                wallet <- failLeft ("No wallet @ " <>) mWallet
                withContext
                    mpfsClient
                    (mkValidation auth)
                    (submit wallet)
                    $ agentCmd (owner wallet) agentCommand
            RetractRequest refId -> do
                wallet <- failLeft ("No wallet @ " <>) mWallet
                let Submission submit' = submit wallet
                fmap txHash $ submit' $ \address ->
                    retractChange address refId
            GetFacts tokenId -> getTokenFacts tokenId
            Wallet walletCommand -> do
                liftIO $ case mWallet of
                    Right wallet -> walletCmd (Right wallet) walletCommand
                    Left walletFile -> walletCmd (Left walletFile) walletCommand

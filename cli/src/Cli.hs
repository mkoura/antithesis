{-# LANGUAGE DeriveGeneric #-}

module Cli
    ( cmd
    , Command (..)
    , Config (..)
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Core.Types
    ( RequestRefId
    , TokenId
    , TxHash
    , Wallet
    , WithTxHash (..)
    )
import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Types qualified as Aeson
import GHC.Generics (Generic)
import MPFS.API (getTokenFacts, retractChange)
import Oracle.Cli (OracleCommand (..), oracleCmd)
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig
    )
import Servant.Client (ClientM)
import Submitting (Submitting, signAndSubmit)
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
import Wallet.Cli (WalletCommand, walletCmd)

data Command a where
    RequesterCommand :: RequesterCommand a -> Command a
    OracleCommand :: OracleCommand a -> Command a
    AgentCommand :: AgentCommand NotReady a -> Command a
    RetractRequest
        :: { outputReference :: RequestRefId
           }
        -> Command TxHash
    GetFacts :: Command JSValue
    Wallet :: WalletCommand a -> Command a

deriving instance Show (Command a)
deriving instance Eq (Command a)

newtype Config = Config
    { agentValidationConfig :: TestRunValidationConfig
    }
    deriving (Show, Eq, Generic)

instance Aeson.FromJSON Config

cmd
    :: Submitting
    -> Either FilePath Wallet
    -> Maybe TokenId
    -> Command a
    -> ClientM a
cmd sbmt mwf tokenId command = do
    cfg <- liftIO $ do
        configFile <- getEnv "ANTI_CONFIG_FILE"
        config <-
            eitherDecodeFileStrict' configFile :: IO (Either String Config)
        case config of
            Left err -> error $ "Failed to parse config file: " ++ err
            Right cfg -> pure cfg
    cmdCore sbmt cfg mwf tokenId command

cmdCore
    :: Submitting
    -> Config
    -> Either FilePath Wallet
    -> Maybe TokenId
    -> Command a
    -> ClientM a
cmdCore sbmt Config{agentValidationConfig} mWallet mTokenId = \case
    RequesterCommand requesterCommand ->
        case (mWallet, mTokenId) of
            (Right wallet, Just tokenId) ->
                requesterCmd sbmt wallet tokenId requesterCommand
            _ -> error "Wallet and TokenId are required for RequesterCommand"
    OracleCommand oracleCommand ->
        case mWallet of
            Right wallet ->
                oracleCmd sbmt wallet agentValidationConfig mTokenId oracleCommand
            Left _ -> error "Wallet is required for OracleCommand"
    AgentCommand agentCommand ->
        case (mWallet, mTokenId) of
            (Right wallet, Just tokenId) ->
                agentCmd sbmt wallet tokenId agentCommand
            _ -> error "Wallet and TokenId are required for AgentCommand"
    RetractRequest refId -> do
        case mWallet of
            Right wallet -> fmap txHash $ signAndSubmit sbmt wallet $ \address ->
                retractChange address refId
            Left _ -> error "Wallet is required for RetractRequest"
    GetFacts -> do
        case mTokenId of
            Just tokenId -> getTokenFacts tokenId
            Nothing -> error "TokenId is required for GetFacts"
    Wallet walletCommand -> do
        case mWallet of
            Right wallet -> liftIO $ walletCmd (Right wallet) walletCommand
            Left walletFile -> liftIO $ walletCmd (Left walletFile) walletCommand

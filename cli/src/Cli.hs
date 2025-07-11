{-# LANGUAGE DeriveGeneric #-}

module Cli
    ( cmd
    , Command (..)
    , Config (..)
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Core.Types
    ( Fact (..)
    , RequestRefId
    , TokenId
    , TxHash
    , Wallet
    , WithTxHash (..)
    , parseFacts
    )
import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Types qualified as Aeson
import GHC.Generics (Generic)
import MPFS.API (getTokenFacts, retractChange)
import Oracle.Cli (OracleCommand (..), oracleCmd)
import Servant.Client (ClientM)
import Submitting (Submitting, signAndSubmit)
import System.Environment (getEnv)
import Text.JSON.Canonical (FromJSON (..), JSValue)
import User.Agent.Cli
    ( AgentCommand (..)
    , IsReady (NotReady)
    , agentCmd
    )
import User.Agent.Validation.Config (AgentValidationConfig)
import User.Requester.Cli
    ( RequesterCommand
    , requesterCmd
    )
import Validation (Validation (..))
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
    { agentValidationConfig :: AgentValidationConfig
    }
    deriving (Show, Eq, Generic)

instance Aeson.FromJSON Config

mkValidation :: TokenId -> Validation ClientM
mkValidation tk =
    Validation
        { mpfsGetFacts = do
            factsObject <- getTokenFacts tk
            case fromJSON factsObject of
                Nothing -> error "Failed to parse facts from JSON"
                Just factsObject' -> do
                    let factsList = parseFacts factsObject'
                    return $ uncurry Fact <$> factsList
        }

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
cmdCore sbmt Config{agentValidationConfig} (Right wallet) (Just tokenId) command = do
    let validation = mkValidation tokenId
    case command of
        RequesterCommand requesterCommand ->
            requesterCmd
                sbmt
                agentValidationConfig
                validation
                wallet
                tokenId
                requesterCommand
        OracleCommand oracleCommand ->
            oracleCmd sbmt wallet (Just tokenId) oracleCommand
        AgentCommand agentCommand ->
            agentCmd
                sbmt
                agentValidationConfig
                validation
                wallet
                tokenId
                agentCommand
        GetFacts -> getTokenFacts tokenId
        RetractRequest refId -> fmap txHash $ signAndSubmit sbmt wallet $ \address ->
            retractChange address refId
        Wallet walletCommand -> liftIO $ walletCmd (Right wallet) walletCommand
cmdCore sbmt _ (Right wallet) Nothing command =
    case command of
        RetractRequest refId -> fmap txHash $ signAndSubmit sbmt wallet $ \address ->
            retractChange address refId
        Wallet walletCommand -> liftIO $ walletCmd (Right wallet) walletCommand
        OracleCommand oracleCommand -> oracleCmd sbmt wallet Nothing oracleCommand
        _ -> error "TokenId is required for this command"
cmdCore _ _iftw mwf@(Left _) (Just tokenId) command =
    case command of
        GetFacts -> getTokenFacts tokenId
        Wallet walletCommand -> liftIO $ walletCmd mwf walletCommand
        _ -> error "Wallet is required for this command"
cmdCore _ _iftw mwf@(Left _) Nothing command =
    case command of
        Wallet walletCommand -> liftIO $ walletCmd mwf walletCommand
        _ -> error "Wallet and TokenId are required for this command"

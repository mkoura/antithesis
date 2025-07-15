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
import Lib.SSH.Private
    ( KeyAPI (..)
    , SSHKeySelector (SSHKeySelector)
    , SigningMap
    )
import MPFS.API (getTokenFacts, retractChange)
import Oracle.Cli (OracleCommand (..), oracleCmd)
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
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

data Config = Config
    { testRunValidationConfig :: TestRunValidationConfig
    , sshKeySelector :: String
    }
    deriving (Show, Eq)

instance Aeson.FromJSON Config where
    parseJSON = Aeson.withObject "Config" $ \v ->
        Config
            <$> v Aeson..: "testRunValidationConfig"
            <*> v Aeson..: "sshKeySelector"

cmd
    :: Submitting
    -> Either FilePath Wallet
    -> Maybe SigningMap
    -> Maybe TokenId
    -> Command a
    -> ClientM a
cmd sbmt mwf msign tokenId command = do
    cfg <- liftIO $ do
        configFile <- getEnv "ANTI_CONFIG_FILE"
        config <-
            eitherDecodeFileStrict' configFile :: IO (Either String Config)
        case config of
            Left err -> error $ "Failed to parse config file: " ++ err
            Right cfg -> pure cfg
    cmdCore sbmt cfg mwf msign tokenId command

failNothing :: Applicative m => [Char] -> Maybe a -> m a
failNothing w Nothing = error w
failNothing _ (Just x) = pure x

failLeft :: Applicative m => (a -> String) -> Either a b -> m b
failLeft f (Left err) = error $ f err
failLeft _ (Right x) = pure x

cmdCore
    :: Submitting
    -> Config
    -> Either FilePath Wallet
    -> Maybe SigningMap
    -> Maybe TokenId
    -> Command a
    -> ClientM a
cmdCore
    sbmt
    Config{testRunValidationConfig, sshKeySelector}
    mWallet
    mSigning
    mTokenId = \case
        RequesterCommand requesterCommand -> do
            signing <- failNothing "No SSH file" mSigning
            keyAPI <-
                failNothing "No SSH selector"
                    $ signing
                    $ SSHKeySelector sshKeySelector
            tokenId <- failNothing "No TokenId" mTokenId
            wallet <- failLeft ("No wallet @ " <>) mWallet
            requesterCmd sbmt wallet tokenId (sign keyAPI) requesterCommand
        OracleCommand oracleCommand -> do
            wallet <- failLeft ("No wallet @ " <>) mWallet
            oracleCmd sbmt wallet testRunValidationConfig mTokenId oracleCommand
        AgentCommand agentCommand -> do
            tokenId <- failNothing "No TokenId" mTokenId
            wallet <- failLeft ("No wallet @ " <>) mWallet
            agentCmd sbmt wallet tokenId agentCommand
        RetractRequest refId -> do
            wallet <- failLeft ("No wallet @ " <>) mWallet
            fmap txHash $ signAndSubmit sbmt wallet $ \address ->
                retractChange address refId
        GetFacts -> do
            tokenId <- failNothing "No TokenId" mTokenId
            getTokenFacts tokenId
        Wallet walletCommand -> do
            liftIO $ case mWallet of
                Right wallet -> walletCmd (Right wallet) walletCommand
                Left walletFile -> walletCmd (Left walletFile) walletCommand

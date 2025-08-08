module App
    ( client
    , Result (..)
    ) where

import Cli (cmd)
import Control.Exception (SomeException (SomeException), catch, try)
import Core.Types.Basic (TokenId (..))
import Data.ByteString.Char8 qualified as B
import Lib.Box (Box (..))
import Lib.SSH.Private (decodePrivateSSHFile)
import Network.HTTP.Client
    ( ManagerSettings (..)
    , Request (requestBody)
    , RequestBody (..)
    , defaultManagerSettings
    , newManager
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options (Options (..), parseArgs)
import Servant.Client
    ( BaseUrl (..)
    , ClientError
    , ClientM
    , Scheme (..)
    , mkClientEnv
    , parseBaseUrl
    , runClientM
    )
import Submitting
    ( IfToWait (..)
    , Submitting (..)
    , readWallet
    , signAndSubmitMPFS
    )
import System.Environment (getArgs, getEnv, lookupEnv)
import Text.JSON.Canonical (JSValue, ToJSON (..))

data Result
    = Success
        { options :: Box Options
        , walletFile :: FilePath
        , mpfsHost :: String
        , result :: Either ClientError JSValue
        }
    | Failure SomeException
    | Help
    deriving (Show)

client
    :: IO Result
client = do
    args <- getArgs
    fc <- try $ parseArgs args
    case fc of
        Left (SomeException _) -> return Help
        Right o@(Box (Options command)) -> do
            let action = do
                    mpfs_host <- getEnv "ANTI_MPFS_HOST"
                    mtk <- fmap TokenId <$> lookupEnv "ANTI_TOKEN_ID"
                    baseUrl <- parseBaseUrl mpfs_host
                    walletFile <- getEnv "ANTI_WALLET_FILE"
                    mWallet <-
                        (Right <$> readWallet walletFile)
                            `catch` \(_ :: IOError) -> return $ Left walletFile
                    mSigning <- do
                        mf <- lookupEnv "ANTI_SSH_FILE"
                        case mf of
                            Just f -> do
                                pw <- getEnv "ANTI_SSH_PASSWORD"
                                r <- decodePrivateSSHFile (B.pack pw) f
                                return $ Just r
                            Nothing -> return Nothing
                    manger <-
                        newManager
                            $ if baseUrlScheme baseUrl == Https
                                then tlsManagerSettings
                                else defaultManagerSettings
                    let clientEnv = mkClientEnv manger baseUrl
                    eiftw <- lookupEnv "ANTI_WAIT"
                    let iftw = case eiftw of
                            Just s -> Wait (read s)
                            Nothing -> NoWait
                        runClient :: forall a. ClientM a -> IO a
                        runClient c = do
                            e <- runClientM c clientEnv
                            case e of
                                Left err -> error $ "Client error: " ++ show err
                                Right r -> return r
                    let sbmt = Submitting{ifToWait = iftw, runClient}
                        submit = signAndSubmitMPFS sbmt
                    Success o walletFile mpfs_host
                        <$> runClientM
                            (cmd submit mWallet mSigning mtk command >>= toJSON)
                            clientEnv
            action `catch` (return . Failure)

_logRequests :: ManagerSettings -> ManagerSettings
_logRequests settings =
    settings
        { managerModifyRequest =
            \req -> do
                print req
                case requestBody req of
                    RequestBodyLBS body -> do
                        putStrLn $ "Request Body: " ++ show body
                    _ -> return ()
                return req
        }

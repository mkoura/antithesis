module App (server) where

import Cli (cmd)
import Core.Types
    ( TokenId (..)
    )
import Lib.Box (Box (..))
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
    , Scheme (..)
    , mkClientEnv
    , parseBaseUrl
    , runClientM
    )
import Submitting (readWallet)
import System.Environment (getArgs, getEnv, lookupEnv)
import Text.JSON.Canonical (JSValue, ToJSON (..))

server :: IO (Box Options, Either ClientError JSValue)
server = do
    args <- getArgs
    o@(Box (Options command)) <- parseArgs args
    mpfs_host <- getEnv "ANTI_MPFS_HOST"
    mtk <- fmap TokenId <$> lookupEnv "ANTI_TOKEN_ID"
    baseUrl <- parseBaseUrl mpfs_host
    walletFile <- getEnv "ANTI_WALLET_FILE"
    wallet <- readWallet walletFile
    manger <-
        newManager
            $ if baseUrlScheme baseUrl == Https
                then tlsManagerSettings
                else defaultManagerSettings
    let clientEnv = mkClientEnv manger baseUrl
    (o,) <$> runClientM (cmd wallet mtk command >>= toJSON) clientEnv

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

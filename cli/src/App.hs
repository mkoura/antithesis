module App
    ( client
    , Result (..)
    ) where

import Cli (cmd)
import Control.Exception (SomeException (SomeException), catch, try)
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
import Paths_anti (version)
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
    , signAndSubmitMPFS
    )
import System.Environment (getEnv, lookupEnv)
import Text.JSON.Canonical (JSValue, ToJSON (..))

data Result
    = Success
        { options :: Box Options
        , mpfsHost :: String
        , result :: Either ClientError JSValue
        }
    | Failure SomeException
    | Help
    deriving (Show)

client
    :: IO Result
client = do
    fc <- try $ parseArgs version
    case fc of
        Left (SomeException _) -> return Help
        Right o@(Box (Options command)) -> do
            let action = do
                    mpfs_host <- getEnv "ANTI_MPFS_HOST"
                    eiftw <- lookupEnv "ANTI_WAIT"
                    baseUrl <- parseBaseUrl mpfs_host
                    manger <-
                        newManager
                            $ if baseUrlScheme baseUrl == Https
                                then tlsManagerSettings
                                else defaultManagerSettings
                    let clientEnv = mkClientEnv manger baseUrl
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
                    Success o mpfs_host
                        <$> runClientM
                            (cmd submit command >>= toJSON)
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

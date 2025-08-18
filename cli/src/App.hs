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
    )
import Options (Options (..), parseArgs)
import Paths_anti (version)
import Text.JSON.Canonical (JSValue, ToJSON (..))

data Result
    = Success
        { result :: JSValue
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
        Right (Box (Options command)) -> do
            let action = Success <$> (cmd command >>= toJSON)
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module App (server) where

import Cli (cmd)
import Data.Aeson (Value)
import Network.HTTP.Client
    ( ManagerSettings (..)
    , Request (requestBody)
    , RequestBody (..)
    , defaultManagerSettings
    , newManager
    )
import Options (parseArgs)
import Servant.Client
    ( BaseUrl
        ( BaseUrl
        , baseUrlHost
        , baseUrlPath
        , baseUrlPort
        , baseUrlScheme
        )
    , ClientError
    , Scheme (Http)
    , mkClientEnv
    , runClientM
    )
import System.Environment (getArgs)
import Types (Host (..), Options (Options), Port (..))

server :: IO (Options, Either ClientError Value)
server = do
    args <- getArgs
    o@(Options (Host host') (Port port) command) <- parseArgs args
    -- putStrLn $ "Options: " ++ show o
    manger <- newManager defaultManagerSettings
    let baseUrl =
            BaseUrl
                { baseUrlScheme = Http
                , baseUrlHost = host'
                , baseUrlPort = port
                , baseUrlPath = ""
                }
        clientEnv = mkClientEnv manger baseUrl
    (o,) <$> runClientM (cmd command) clientEnv

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

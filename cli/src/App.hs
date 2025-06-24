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
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options (parseArgs)
import Servant.Client
    ( BaseUrl (..)
    , ClientError
    , Scheme (..)
    , mkClientEnv
    , parseBaseUrl
    , runClientM
    )
import System.Environment (getArgs, getEnv)
import Types
    ( Address (Address)
    , Options (Options)
    , TokenId (..)
    , Wallet (..)
    )

server :: IO (Options, Either ClientError Value)
server = do
    args <- getArgs
    o@(Options command) <- parseArgs args
    mpfs_host <- getEnv "ANTI_MPFS_HOST"
    tk <- TokenId <$> getEnv "ANTI_TOKEN_ID"
    baseUrl <- parseBaseUrl mpfs_host
    let wallet =
            Wallet
                { address =
                    Address
                        "addr_test1qpz4m024uhxjnmpj8xhex8hvkvxt99087eq986a2umzfdvgz696pmcla67dtudugpfragnq49lufk7hdgpjszv44h8ssrg0uxk" -- Replace with actual wallet address
                , sign =
                    const
                        "84a300d9010281825820a35167e6f934880c48ce8068c50688bfd4c223f4cb3bb58814b3477b6994173d000182a300581d703cb20fb6259ed583c7b7616d4776c4c8c115fb9d335332516d6047b2011a001e8480028201d81858b8d8799fd8799fd8799f5820bac25301f5d8955439d374e6d5b6c7c2dc60e2783fa9fb61be7f128425e4572fff581c455dbd55e5cd29ec3239af931eecb30cb295e7f64053ebaae6c496b15f584072656769737465722d757365722f6769746875622f70616f6c696e6f2f4141414143334e7a6143316c5a4449314e54453541414141494f3737334a48716c794c58216d35587a4f6a53652b513579464a794c46754d4c4c362b6e363374347437485238ffd8799f40ffffff82583900455dbd55e5cd29ec3239af931eecb30cb295e7f64053ebaae6c496b102d1741de3fdd79abe37880a47d44c152ff89b7aed40650132b5b9e11b0000000253eab0f7021a0002ae89a10081825820d8483b3bf6134fcb8a5abe12fc5a3eaad47c34c52ed5368b22d35674f4fd51cf5840ce5cb3707ec518bc534bd883e83fb1204be39647b10300a5dc1e3da06605be365b2cad240fef8a7544b8a4c652b3c67666f9603ae502a41e47936aec54fdda04f5f6"
                }
    manger <-
        newManager
            $ if baseUrlScheme baseUrl == Https
                then tlsManagerSettings
                else defaultManagerSettings
    let clientEnv = mkClientEnv manger baseUrl
    (o,) <$> runClientM (cmd wallet tk command) clientEnv

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

module Anti.CliSpec
    ( spec
    , runDummyServer
    , anti
    )
where

import Anti.Server (appDummy)
import App (server)
import Cli (Command (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Core.Options (Options (..))
import Core.Types
    ( Directory (..)
    , Platform (..)
    , PublicKeyHash (..)
    , Repository (..)
    , RequestRefId (..)
    , Role (..)
    , SHA1 (..)
    , Username (..)
    )
import Data.Aeson (ToJSON (..), Value, (.=))
import Data.Aeson.Types (object)
import Network.Wai.Handler.Warp (run)
import Oracle.Cli (OracleCommand (..))
import Oracle.Token.Cli (TokenCommand (..))
import System.Environment (setEnv, withArgs)
import Test.Hspec
    ( Spec
    , beforeAll_
    , it
    , shouldReturn
    , xit
    )
import User.Cli (UserCommand (..))
import User.Requester.Cli (RequesterCommand (..))

runDummyServer :: IO ()
runDummyServer = do
    _ <- async $ do
        run 8084 appDummy
    threadDelay 1000000
    setEnv "ANTI_MPFS_HOST" "http://localhost:8084"
    setEnv "ANTI_TOKEN_ID" "dummyTokenId"
    return ()

anti :: [String] -> IO (Options, Value)
anti args = do
    -- Simulate the command line arguments
    -- Call the main function with the simulated arguments
    ev <- withArgs args server
    case ev of
        (_, Left err) -> error $ "Error: " ++ show err
        (o, Right result) -> return (o, result)

dummyTxHash :: Value
dummyTxHash =
    object
        ["txHash" .= ("dummyTransactionId" :: String), "value" .= toJSON ()]

spec :: Spec
spec = beforeAll_ runDummyServer $ do
    xit "can request user registration" $ do
        let args =
                [ "user"
                , "request"
                , "register-public-key"
                , "--platform"
                , "github"
                , "--username"
                , "paolino"
                , "--pubkeyhash"
                , "AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8"
                ]
        let opts =
                Options
                    { optionsCommand =
                        UserCommand
                            $ UserRequesterCommand
                                RegisterPublicKey
                                    { platform = Platform "github"
                                    , username = Username "paolino"
                                    , pubkeyhash =
                                        PublicKeyHash
                                            "AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8"
                                    }
                    }
        anti args
            `shouldReturn` (opts, toJSON dummyTxHash)
    xit "can request user unregistration" $ do
        let args =
                [ "user"
                , "request"
                , "unregister-public-key"
                , "--platform"
                , "github"
                , "--username"
                , "bob"
                , "--pubkeyhash"
                , "607a0d8a64616a407537edf0d9b59cf4cb509c556f6d2de4250ce15df2"
                ]

        let opts =
                Options
                    { optionsCommand =
                        UserCommand
                            $ UserRequesterCommand
                                UnregisterPublicKey
                                    { platform = Platform "github"
                                    , username = Username "bob"
                                    , pubkeyhash =
                                        PublicKeyHash
                                            "607a0d8a64616a407537edf0d9b59cf4cb509c556f6d2de4250ce15df2"
                                    }
                    }
        anti args `shouldReturn` (opts, toJSON dummyTxHash)

    xit "can request removing user from a project" $ do
        let args =
                [ "user"
                , "request"
                , "unregister-role"
                , "--platform"
                , "github"
                , "--repository"
                , "cardano-foundation/antithesis"
                , "--role"
                , "maintainer"
                , "--username"
                , "bob"
                ]
        let opts =
                Options
                    { optionsCommand =
                        UserCommand
                            $ UserRequesterCommand
                                UnregisterRole
                                    { platform = Platform "github"
                                    , repository = Repository "cardano-foundation" "antithesis"
                                    , role = Role "maintainer"
                                    , username = Username "bob"
                                    }
                    }

        anti args `shouldReturn` (opts, toJSON dummyTxHash)

    xit "can request antithesis run" $ do
        let args =
                [ "user"
                , "request"
                , "test"
                , "--platform"
                , "github"
                , "--repository"
                , "cardano-foundation/antithesis"
                , "--username"
                , "bob"
                , "--commit"
                , "9114528e2343e6fcf3c92de71364275227e6b16d"
                ]
        let opts =
                Options
                    { optionsCommand =
                        UserCommand
                            $ UserRequesterCommand
                                RequestTest
                                    { platform = Platform "github"
                                    , repository = Repository "cardano-foundation" "antithesis"
                                    , username = Username "bob"
                                    , commit = SHA1 "9114528e2343e6fcf3c92de71364275227e6b16d"
                                    , directory = Directory "."
                                    }
                    }
        anti args `shouldReturn` (opts, toJSON dummyTxHash)

    xit "can retract a request" $ do
        let args =
                [ "user"
                , "request"
                , "retract"
                , "--outref"
                , "9114528e2343e6fcf3c92de71364275227e6b16d-0"
                ]
        let opts =
                Options
                    { optionsCommand =
                        UserCommand
                            $ RetractRequest
                                { outputReference =
                                    RequestRefId
                                        "9114528e2343e6fcf3c92de71364275227e6b16d-0"
                                }
                    }
        anti args `shouldReturn` (opts, toJSON dummyTxHash)

    it "can get a token" $ do
        let args =
                [ "oracle"
                , "token"
                , "get"
                ]
        let opts =
                Options
                    { optionsCommand =
                        OracleCommand $ OracleTokenCommand GetToken
                    }
        anti args
            `shouldReturn` ( opts
                           , toJSON ()
                           )

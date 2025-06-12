{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Anti.CliSpec
    ( spec
    , runDummyServer
    , anti
    )
where

import App (server)
import Anti.Server (appDummy, dummyTxId)
import Types
    ( Command (..)
    , Directory (..)
    , Host (..)
    , Options (..)
    , OracleCommand (..)
    , OutputReference (..)
    , Platform (..)
    , Port (..)
    , PublicKeyHash (..)
    , Repository (..)
    , RequesterCommand (..)
    , Role (..)
    , SHA1 (..)
    , TokenId (..)
    , UserCommand (..)
    , TokenCommand (..)
    , Username (..)
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Data.Aeson (Value, (.=))
import Data.Aeson.Types (object)
import Network.Wai.Handler.Warp (run)
import System.Environment (withArgs)
import Test.Hspec
    ( Spec
    , beforeAll_
    , it
    , shouldReturn
    )

runDummyServer :: IO ()
runDummyServer = do
    _ <- async $ do
        run 8084 appDummy
    threadDelay 1000000
    return ()

anti :: [String] -> IO (Options, Value)
anti args = do
    -- Simulate the command line arguments
    let args' =
            [ "--host"
            , "localhost"
            , "--port"
            , "8084"
            ]
                ++ args
    -- Call the main function with the simulated arguments
    ev <- withArgs args' server
    case ev of
        (_, Left err) -> error $ "Error: " ++ show err
        (o, Right result) -> return (o, result)

spec :: Spec
spec = beforeAll_ runDummyServer $ do
    it "can request user registration" $ do
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
                , "--token-id"
                , "dummyTokenId"
                ]
        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        UserCommand $ UserRequesterCommand
                            RegisterPublicKey
                                { platform = Platform "github"
                                , username = Username "paolino"
                                , pubkeyhash =
                                    PublicKeyHash
                                        "AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8"
                                , tokenId = TokenId "dummyTokenId"
                                }
                    }
        anti args
            `shouldReturn` (opts, dummyTxId)
    it "can request user unregistration" $ do
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
                , "--token-id"
                , "dummyTokenId"
                ]

        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        UserCommand $ UserRequesterCommand
                            UnregisterPublicKey
                                { platform = Platform "github"
                                , username = Username "bob"
                                , pubkeyhash =
                                    PublicKeyHash
                                        "607a0d8a64616a407537edf0d9b59cf4cb509c556f6d2de4250ce15df2"
                                , tokenId = TokenId "dummyTokenId"
                                }
                    }
        anti args `shouldReturn` (opts, dummyTxId)

    it "can request adding user to a project" $ do
        let args =
                [ "user"
                , "request"
                , "register-role"
                , "--platform"
                , "github"
                , "--repository"
                , "cardano-foundation/antithesis"
                , "--role"
                , "maintainer"
                , "--username"
                , "bob"
                , "--token-id"
                , "dummyTokenId"
                ]
        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        UserCommand $ UserRequesterCommand
                            RegisterRole
                                { platform = Platform "github"
                                , repository = Repository "cardano-foundation" "antithesis"
                                , role = Role "maintainer"
                                , username = Username "bob"
                                , tokenId = TokenId "dummyTokenId"
                                }
                    }

        anti args `shouldReturn` (opts, dummyTxId)

    it "can request removing user from a project" $ do
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
                , "--token-id"
                , "dummyTokenId"
                ]
        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        UserCommand $ UserRequesterCommand
                            UnregisterRole
                                { platform = Platform "github"
                                , repository = Repository "cardano-foundation" "antithesis"
                                , role = Role "maintainer"
                                , username = Username "bob"
                                , tokenId = TokenId "dummyTokenId"
                                }
                    }

        anti args `shouldReturn` (opts, dummyTxId)

    it "can request antithesis run" $ do
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
                , "--token-id"
                , "dummyTokenId"
                ]
        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        UserCommand $ UserRequesterCommand
                            RequestTest
                                { platform = Platform "github"
                                , repository = Repository "cardano-foundation" "antithesis"
                                , username = Username "bob"
                                , commit = SHA1 "9114528e2343e6fcf3c92de71364275227e6b16d"
                                , directory = Directory "."
                                , tokenId = TokenId "dummyTokenId"
                                }
                    }
        anti args `shouldReturn` (opts, dummyTxId)
    it "can retract a request" $ do
        let args =
                [ "user"
                , "request"
                , "retract"
                , "--outref"
                , "9114528e2343e6fcf3c92de71364275227e6b16d-0"
                ]
        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        UserCommand $ UserRequesterCommand
                            RetractRequest
                                { outputReference =
                                    OutputReference
                                        { outputReferenceTx = "9114528e2343e6fcf3c92de71364275227e6b16d"
                                        , outputReferenceIndex = 0
                                        }
                                }
                    }
        anti args `shouldReturn` (opts, dummyTxId)
    it "can create a token" $ do
        let args =
                [ "oracle"
                , "token"
                , "create"
                ]
        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        OracleCommand $ OracleTokenCommand
                            CreateToken
                    }
        anti args
            `shouldReturn` ( opts
                           , object
                                [ "tokenId" .= ("dummyTokenId" :: String)
                                ]
                           )
    it "can delete a token" $ do
        let args =
                [ "oracle"
                , "token"
                , "delete"
                , "--token-id"
                , "dummyTokenId"
                ]
        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        OracleCommand $ OracleTokenCommand
                            (DeleteToken $ TokenId "dummyTokenId")
                    }
        anti args `shouldReturn` (opts, dummyTxId)
    it "can get a token" $ do
        let args =
                [ "oracle"
                , "token"
                , "get"
                , "--token-id"
                , "dummyTokenId"
                ]
        let opts =
                Options
                    { host = Host "localhost"
                    , port = Port 8084
                    , command =
                        OracleCommand $ OracleTokenCommand
                            (GetToken $ TokenId "dummyTokenId")
                    }
        anti args
            `shouldReturn` ( opts
                           , object
                                [ "tokenId" .= ("dummyTokenId" :: String)
                                ]
                           )

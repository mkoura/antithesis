module Options
    ( Command (..)
    , commandParser
    , Options (..)
    , optionsParser
    , parseArgs
    ) where

import Cli (Command (..))
import Core.Options (outputReferenceParser, tokenIdOption)
import Data.Version (Version)
import Lib.Box (Box (..), fmapBox)
import OptEnvConf
    ( Parser
    , command
    , commands
    , runParser
    )
import Oracle.Options (oracleCommandParser)
import User.Agent.Options (agentCommandParser)
import User.Requester.Options (requesterCommandParser)
import Wallet.Options (walletCommandParser)

newtype Options a = Options
    { optionsCommand :: Command a
    }
    deriving (Eq, Show)

commandParser :: Parser (Box Command)
commandParser =
    commands
        [ command "oracle" "Manage oracle operations"
            $ fmapBox OracleCommand <$> oracleCommandParser
        , command "requester" "Manage requester operations"
            $ fmapBox RequesterCommand <$> requesterCommandParser
        , command
            "retract"
            "Retract a request"
            retractRequestOptions
        , command "facts" "Get token facts"
            $ Box . GetFacts <$> tokenIdOption
        , command "agent" "Manage agent operations"
            $ fmapBox AgentCommand <$> agentCommandParser
        , command "wallet" "Manage wallet operations"
            $ fmapBox Wallet <$> walletCommandParser
        ]

optionsParser :: Parser (Box Options)
optionsParser = fmapBox Options <$> commandParser

parseArgs :: Version -> IO (Box Options)
parseArgs version =
    runParser
        version
        "anti - A tool for managing Antithesis test runs"
        optionsParser

retractRequestOptions :: Parser (Box Command)
retractRequestOptions =
    Box . RetractRequest
        <$> outputReferenceParser

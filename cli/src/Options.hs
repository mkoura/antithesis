module Options
    ( Command (..)
    , commandParser
    , Options (..)
    , optionsParser
    , parseArgs
    ) where

import Cli (Command (..))
import Core.Options (outputReferenceParser)
import Options.Applicative
    ( Parser
    , command
    , defaultPrefs
    , execParserPure
    , fullDesc
    , handleParseResult
    , header
    , helper
    , hsubparser
    , info
    , progDesc
    , (<**>)
    )
import Oracle.Options (oracleCommandParser)
import User.Agent.Options (agentCommandParser)
import User.Requester.Options (requesterCommandParser)

newtype Options = Options
    { optionsCommand :: Command
    }
    deriving (Eq, Show)

commandParser :: Parser Command
commandParser =
    hsubparser
        ( command
            "oracle"
            ( info
                (OracleCommand <$> oracleCommandParser)
                (progDesc "Manage token updates")
            )
            <> command
                "requester"
                ( info
                    (RequesterCommand <$> requesterCommandParser)
                    (progDesc "Manage requester changes")
                )
            <> command
                "retract"
                ( info
                    retractRequestOptions
                    (progDesc "Retract a change")
                )
            <> command
                "get-facts"
                ( info
                    (pure GetFacts)
                    (progDesc "Get token facts")
                )
            <> command
                "agent"
                ( info
                    (AgentCommand <$> agentCommandParser)
                    (progDesc "Manage agent changes")
                )
        )

optionsParser :: Parser Options
optionsParser = Options <$> commandParser

parseArgs :: [String] -> IO Options
parseArgs args = handleParseResult $ execParserPure defaultPrefs opts args
  where
    opts =
        info
            (optionsParser <**> helper)
            ( fullDesc
                <> progDesc "Antithesis CLI"
                <> header "anti - A tool for managing Antithesis test runs"
            )

retractRequestOptions :: Parser Command
retractRequestOptions =
    RetractRequest
        <$> outputReferenceParser

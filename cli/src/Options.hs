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
                (progDesc "Oracle services")
            )
            <> command
                "requester"
                ( info
                    (RequesterCommand <$> requesterCommandParser)
                    (progDesc "Allow users to send requests")
                )
            <> command
                "retract"
                ( info
                    retractRequestOptions
                    (progDesc "Retract a request")
                )
            <> command
                "get-facts"
                ( info
                    (pure GetFacts)
                    (progDesc "Get token facts")
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

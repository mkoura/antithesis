module Options
    ( Command (..)
    , commandParser
    , Options (..)
    , optionsParser
    , parseArgs
    ) where

import Cli (Command (..))
import Core.Options (outputReferenceParser)
import Lib.Box (Box (..), fmapBox)
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

newtype Options a = Options
    { optionsCommand :: Command a
    }
    deriving (Eq, Show)

commandParser :: (Box Command -> b) -> Parser b
commandParser f =
    hsubparser
        ( command
            "oracle"
            ( info
                (f <$> oracleCommandParser (fmapBox OracleCommand))
                (progDesc "Manage token updates")
            )
            <> command
                "requester"
                ( info
                    (f <$> requesterCommandParser (fmapBox RequesterCommand))
                    (progDesc "Manage requester changes")
                )
            <> command
                "retract"
                ( info
                    (f <$> retractRequestOptions)
                    (progDesc "Retract a change")
                )
            <> command
                "get-facts"
                ( info
                    (pure . f . Box $ GetFacts)
                    (progDesc "Get token facts")
                )
            <> command
                "agent"
                ( info
                    (f <$> agentCommandParser (fmapBox undefined))
                    (progDesc "Manage agent changes")
                )
        )

optionsParser :: Parser (Box Options)
optionsParser = commandParser $ fmapBox Options

parseArgs :: [String] -> IO (Box Options)
parseArgs args = handleParseResult $ execParserPure defaultPrefs opts args
  where
    opts =
        info
            (optionsParser <**> helper)
            ( fullDesc
                <> progDesc "Antithesis CLI"
                <> header "anti - A tool for managing Antithesis test runs"
            )

retractRequestOptions :: Parser (Box Command)
retractRequestOptions =
    Box . RetractRequest
        <$> outputReferenceParser

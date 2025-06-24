module Options
    ( Command (..)
    , commandParser
    , Options (..)
    , optionsParser
    , parseArgs
    ) where

import Cli (Command (..))
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
import User.Options (userCommandParser)

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
                (OracleCommand <$> oracleCommandParser <**> helper)
                (progDesc "Oracle services")
            )
            <> command
                "user"
                ( info
                    (UserCommand <$> userCommandParser <**> helper)
                    (progDesc "Manage user requests")
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

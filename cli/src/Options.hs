module Options
    ( Command (..)
    , commandParser
    , Options (..)
    , optionsParser
    , parseArgs
    ) where

import Cli (Command (..))
import Core.Options (outputReferenceParser, tokenIdOption)
import Core.Types.Basic (TokenId)
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
import Wallet.Options (walletCommandParser)

newtype Options a = Options
    { optionsCommand :: Command a
    }
    deriving (Eq, Show)

commandParser :: Maybe TokenId -> Parser (Box Command)
commandParser ptk =
    hsubparser
        ( command
            "oracle"
            ( info
                (fmapBox OracleCommand <$> oracleCommandParser ptk)
                (progDesc "Manage token updates")
            )
            <> command
                "requester"
                ( info
                    (fmapBox RequesterCommand <$> requesterCommandParser ptk)
                    (progDesc "Manage requester changes")
                )
            <> command
                "retract"
                ( info
                    retractRequestOptions
                    (progDesc "Retract a change")
                )
            <> command
                "facts"
                ( info
                    (Box . GetFacts <$> tokenIdOption ptk)
                    (progDesc "Get token facts")
                )
            <> command
                "agent"
                ( info
                    (fmapBox AgentCommand <$> agentCommandParser ptk)
                    (progDesc "Manage agent changes")
                )
            <> command
                "wallet"
                ( info
                    (fmapBox Wallet <$> walletCommandParser)
                    (progDesc "Manage wallet operations")
                )
        )

optionsParser :: Maybe TokenId -> Parser (Box Options)
optionsParser ptk = fmapBox Options <$> commandParser ptk

parseArgs :: [String] -> Maybe TokenId -> IO (Box Options)
parseArgs args ptk = handleParseResult $ execParserPure defaultPrefs opts args
  where
    opts =
        info
            (optionsParser ptk <**> helper)
            ( fullDesc
                <> progDesc "Antithesis CLI"
                <> header "anti - A tool for managing Antithesis test runs"
            )

retractRequestOptions :: Parser (Box Command)
retractRequestOptions =
    Box . RetractRequest
        <$> outputReferenceParser

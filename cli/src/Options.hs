{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Options
    ( Command (..)
    , commandParser
    , Options (..)
    , optionsParser
    , parseArgs
    ) where

import Cli (Command (..))
import Control.Applicative (optional)
import Core.Options
    ( outputReferenceParser
    , tokenIdOption
    , walletOption
    )
import Core.Types.MPFS (mpfsClientOption)
import Core.Types.Mnemonics.Options (queryConsole)
import Data.ByteString.Char8 qualified as B
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Version (Version)
import Facts (FactsSelection (..), TestRunSelection (..))
import GitHub (Auth (..))
import Lib.Box (Box (..), fmapBox)
import MPFS.API (mpfsClient)
import OptEnvConf
    ( Parser
    , auto
    , command
    , commands
    , env
    , help
    , long
    , mapIO
    , metavar
    , option
    , reader
    , runParser
    , setting
    , str
    , (<|>)
    )
import Oracle.Options (oracleCommandParser)
import User.Agent.Options (agentCommandParser)
import User.Requester.Options (requesterCommandParser)
import Wallet.Options (walletCommandParser)

newtype Options a = Options
    { optionsCommand :: Command a
    }

githubAuthOption :: Parser Auth
githubAuthOption =
    fmap OAuth
        $ mapIO id
        $ setting
            [ help "Prompt for the passphrase for the encrypted mnemonics"
            , env "ANTI_INTERACTIVE_PASSWORD"
            , metavar "NONE"
            , long "ask-passphrase"
            , option
            , reader
                $ str @String
                    $> ( T.encodeUtf8
                            <$> queryConsole "Enter your GitHub personal access token: "
                       )
            ]
        <|> setting
            [ env "GITHUB_PERSONAL_ACCESS_TOKEN"
            , metavar "PASSPHRASE"
            , help
                "A GitHub personal access token with access to public repositories"
            , reader $ fmap (pure . B.pack) str
            ]

commandParser :: Parser (Box Command)
commandParser =
    commands
        [ command "oracle" "Manage oracle operations"
            $ (\a c -> fmapBox (OracleCommand a c))
                <$> githubAuthOption
                <*> mpfsClientOption
                <*> oracleCommandParser
        , command "requester" "Manage requester operations"
            $ (\a c -> fmapBox (RequesterCommand a c))
                <$> githubAuthOption
                <*> mpfsClientOption
                <*> requesterCommandParser
        , command "agent" "Manage agent operations"
            $ (\a c -> fmapBox (AgentCommand a c))
                <$> githubAuthOption
                <*> mpfsClientOption
                <*> agentCommandParser
        , command "wallet" "Manage wallet operations"
            $ fmapBox Wallet <$> walletCommandParser
        , command
            "retract"
            "Retract a request"
            retractRequestOptions
        , command "facts" "Get token facts"
            $ (\c tk -> fmapBox (GetFacts c tk))
                <$> mpfsClientOption
                <*> tokenIdOption
                <*> factsSelectionParser
        , command "token" "Get the token content"
            $ (\a -> fmap Box . GetToken a)
                <$> githubAuthOption
                <*> mpfsClientOption
                <*> tokenIdOption
        ]

factsSelectionParser :: Parser (Box FactsSelection)
factsSelectionParser =
    commands
        [ command "user" "Get registered users" (pure $ Box UserFacts)
        , command "role" "Get registered roles" (pure $ Box RoleFacts)
        , command
            "test-run"
            "Get test runs"
            (fmapBox TestRunFacts <$> testRunSelectionParser)
        , command
            "config"
            "Get the oracle configuration"
            (pure $ Box ConfigFact)
        , command
            "white-listed"
            "Get white-listed repositories"
            (pure $ Box WhiteListedFacts)
        ]
        <|> pure (Box AllFacts)

testRunSelectionParser :: Parser (Box TestRunSelection)
testRunSelectionParser =
    commands
        [ command "pending" "Get pending test runs" (pure $ Box TestRunPending)
        , command "running" "Get running test runs" (pure $ Box TestRunRunning)
        , command "done" "Get done test runs" (pure $ Box TestRunDone)
        , command
            "rejected"
            "Get rejected test runs"
            (pure $ Box TestRunRejected)
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
    fmap (fmap Box) . RetractRequest
        <$> mpfsClientOption
        <*> walletOption
        <*> outputReferenceParser

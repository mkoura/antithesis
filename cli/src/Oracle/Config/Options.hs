{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Oracle.Config.Options
    ( configCommandParser
    , Box (..)
    )
where

import Core.Options (tokenIdOption)
import Core.Types.Basic (Owner (..), TokenId)
import Lib.Box (Box (..))
import Options.Applicative
    ( Parser
    , auto
    , command
    , help
    , hsubparser
    , info
    , long
    , metavar
    , option
    , progDesc
    , strOption
    )
import Oracle.Config.Cli (ConfigCmd (..))
import Oracle.Config.Types (Config (..))
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )

configOption :: Parser Config
configOption = do
    minDuration <-
        option
            auto
            ( long "min-test-duration"
                <> metavar "MIN_TEST_HOURS"
                <> help "Minimum duration of the tests in hours"
            )
    maxDuration <-
        option
            auto
            ( long "max-test-duration"
                <> metavar "MAX_TEST_HOURS"
                <> help "Maximum duration of the tests in hours"
            )
    agent <-
        Owner
            <$> strOption
                ( long "agent-pkh"
                    <> metavar "AGENT_PUBLIC_KEY_HASH"
                    <> help "Public key hash of the agent that will run the tests"
                )
    pure
        $ Config
            { configAgent = agent
            , configTestRun =
                TestRunValidationConfig
                    { minDuration
                    , maxDuration
                    }
            }

configCommandParser
    :: Maybe TokenId
    -> Parser (Box ConfigCmd)
configCommandParser ptk =
    hsubparser
        ( command
            "set"
            ( info
                ( fmap Box . SetConfig
                    <$> tokenIdOption ptk
                    <*> configOption
                )
                (progDesc "Update the oracle configuration")
            )
        )

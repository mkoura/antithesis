{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

module User.Agent.Options
    ( agentCommandParser
    ) where

import Core.Types.Basic (Duration (..))
import Core.Types.Tx (WithTxHash)
import Lib.Box (Box (..))
import Options.Applicative
    ( Alternative (..)
    , Parser
    , auto
    , command
    , eitherReader
    , help
    , hsubparser
    , info
    , long
    , option
    , progDesc
    , short
    , str
    )
import Oracle.Validate.Requests.TestRun.Update (UpdateTestRunFailure)
import Oracle.Validate.Types (AValidationResult)
import User.Agent.Cli
    ( AgentCommand (..)
    , IsReady (NotReady)
    , TestRunId (..)
    )
import User.Agent.Types (TestRunMap)
import User.Types
    ( Phase (..)
    , TestRunRejection (..)
    , TestRunState
    , URL (..)
    )

agentCommandParser
    :: Parser (Box (AgentCommand NotReady))
agentCommandParser =
    hsubparser
        ( command
            "accept-test"
            ( info
                (Box <$> acceptTestOptions)
                (progDesc "Request a test on a specific platform")
            )
            <> command
                "reject-test"
                ( info
                    (Box <$> rejectTestOptions)
                    (progDesc "Reject a test with a reason")
                )
            <> command
                "report-test"
                ( info
                    (Box <$> reportTestOptions)
                    (progDesc "Report the result of a test run")
                )
            <> command
                "query"
                ( info
                    (Box <$> queryOptions)
                    (progDesc "Query the status of test runs")
                )
        )

queryOptions
    :: Parser (AgentCommand NotReady TestRunMap)
queryOptions =
    pure Query

testRunIdOption
    :: Parser TestRunId
testRunIdOption =
    TestRunId
        <$> option
            str
            ( short 'i'
                <> long "test-run-id"
                <> help "The ID of the test run to accept/reject/report"
            )

acceptTestOptions
    :: Parser
        ( AgentCommand
            NotReady
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState RunningT))
            )
        )
acceptTestOptions =
    Accept <$> testRunIdOption <*> pure ()

testRejectionParser :: Parser TestRunRejection
testRejectionParser =
    option
        (eitherReader readReason)
        (long "reason" <> help "TestRunRejection for rejection")
  where
    readReason :: String -> Either String TestRunRejection
    readReason "broken-instructions" = Right BrokenInstructions
    readReason "unclear-intent" = Right UnclearIntent
    readReason _ = Left "Unknown reason for rejection"

rejectTestOptions
    :: Parser
        ( AgentCommand
            NotReady
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState DoneT))
            )
        )
rejectTestOptions = do
    testRunId <- testRunIdOption
    reason <- many testRejectionParser
    pure $ Reject testRunId () reason

reportTestOptions
    :: Parser
        ( AgentCommand
            NotReady
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState DoneT))
            )
        )
reportTestOptions = do
    testRunId <- testRunIdOption
    duration <-
        Duration
            <$> option
                auto
                (long "duration" <> help "Duration of the test run in hours")
    url <-
        URL <$> option str (long "url" <> help "URL of the test report")
    pure $ Report testRunId () duration url

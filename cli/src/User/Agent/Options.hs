{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

module User.Agent.Options
    ( agentCommandParser
    ) where

import Core.Options (tokenIdOption)
import Core.Types.Basic (Duration (..), TokenId)
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
    :: Maybe TokenId
    -> Parser (Box (AgentCommand NotReady))
agentCommandParser ptk =
    hsubparser
        ( command
            "accept-test"
            ( info
                (Box <$> acceptTestOptions ptk)
                (progDesc "Request a test on a specific platform")
            )
            <> command
                "reject-test"
                ( info
                    (Box <$> rejectTestOptions ptk)
                    (progDesc "Reject a test with a reason")
                )
            <> command
                "report-test"
                ( info
                    (Box <$> reportTestOptions ptk)
                    (progDesc "Report the result of a test run")
                )
            <> command
                "query"
                ( info
                    (Box <$> queryOptions ptk)
                    (progDesc "Query the status of test runs")
                )
        )

queryOptions
    :: Maybe TokenId
    -> Parser (AgentCommand NotReady TestRunMap)
queryOptions ptk = Query <$> tokenIdOption ptk

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
    :: Maybe TokenId
    -> Parser
        ( AgentCommand
            NotReady
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState RunningT))
            )
        )
acceptTestOptions ptk =
    Accept <$> tokenIdOption ptk <*> testRunIdOption <*> pure ()

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
    :: Maybe TokenId
    -> Parser
        ( AgentCommand
            NotReady
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState DoneT))
            )
        )
rejectTestOptions ptk = do
    testRunId <- testRunIdOption
    reason <- many testRejectionParser
    tokenId <- tokenIdOption ptk
    pure $ Reject tokenId testRunId () reason

reportTestOptions
    :: Maybe TokenId
    -> Parser
        ( AgentCommand
            NotReady
            ( AValidationResult
                UpdateTestRunFailure
                (WithTxHash (TestRunState DoneT))
            )
        )
reportTestOptions ptk = do
    tokenId <- tokenIdOption ptk
    testRunId <- testRunIdOption
    duration <-
        Duration
            <$> option
                auto
                (long "duration" <> help "Duration of the test run in hours")
    url <-
        URL <$> option str (long "url" <> help "URL of the test report")
    pure $ Report tokenId testRunId () duration url

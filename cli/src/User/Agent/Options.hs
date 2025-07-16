{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

module User.Agent.Options
    ( agentCommandParser
    ) where

import Core.Options
    ( commitOption
    , directoryOption
    , platformOption
    , repositoryOption
    , tryOption
    , usernameOption
    )
import Core.Types (Duration (..), WithTxHash)
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
    , str
    )
import User.Agent.Cli (AgentCommand (..), IsReady (NotReady))
import User.Agent.Types (TestRunMap)
import User.Types
    ( Phase (..)
    , TestRun (..)
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

acceptTestOptions
    :: Parser (AgentCommand NotReady (WithTxHash (TestRunState RunningT)))
acceptTestOptions =
    fmap (`Accept` ())
        $ TestRun
            <$> platformOption
            <*> repositoryOption
            <*> directoryOption
            <*> commitOption
            <*> tryOption
            <*> usernameOption

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
    :: Parser (AgentCommand NotReady (WithTxHash (TestRunState DoneT)))
rejectTestOptions = do
    testRun <-
        TestRun
            <$> platformOption
            <*> repositoryOption
            <*> directoryOption
            <*> commitOption
            <*> tryOption
            <*> usernameOption
    reason <- many testRejectionParser
    pure $ Reject testRun () reason

reportTestOptions
    :: Parser (AgentCommand NotReady (WithTxHash (TestRunState DoneT)))
reportTestOptions = do
    testRun <-
        TestRun
            <$> platformOption
            <*> repositoryOption
            <*> directoryOption
            <*> commitOption
            <*> tryOption
            <*> usernameOption
    duration <-
        Duration
            <$> option
                auto
                (long "duration" <> help "Duration of the test run in hours")
    url <-
        URL <$> option str (long "url" <> help "URL of the test report")
    pure $ Report testRun () duration url

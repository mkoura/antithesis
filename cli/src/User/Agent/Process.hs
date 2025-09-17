{-
This is a full automated agent process that can be used in the CLI.
The process
- monitors the antithesis recipients email inbox for new test results.
- monitors the running test-run facts
- publish a report-test transaction for each new result found.
-}
{-# LANGUAGE QuasiQuotes #-}

module User.Agent.Process
    ( ProcessOptions (..)
    , agentProcess
    , parseArgs
    ) where

import Cli (Command (..), cmd)
import Control.Applicative (Alternative (..))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Core.Options (tokenIdOption, walletOption)
import Core.Types.Basic (Duration, TokenId)
import Core.Types.Fact (Fact (..))
import Core.Types.MPFS (MPFSClient (..), mpfsClientOption)
import Core.Types.Tx (WithTxHash)
import Core.Types.Wallet (Wallet)
import Data.Foldable (find, for_)
import Data.Maybe (mapMaybe)
import Data.String.QQ (s)
import Data.Text qualified as T
import Facts (All (..), FactsSelection (..), TestRunSelection (..))
import GitHub (Auth)
import OptEnvConf
    ( Parser
    , runParser
    , withYamlConfig
    )
import Options (githubAuthOption, secretsFileOption)
import Oracle.Process (pollIntervalOption)
import Oracle.Validate.Types (AValidationResult (..))
import Paths_anti (version)
import Text.JSON.Canonical
    ( FromJSON (..)
    )
import User.Agent.Cli
    ( AgentCommand (..)
    , ReportFailure
    )
import User.Agent.Lib (testRunDuration)
import User.Agent.Options
    ( agentEmailOption
    , agentEmailPasswordOption
    , daysOption
    )
import User.Agent.PublishResults.Email
    ( EmailPassword
    , EmailUser
    , Result (..)
    )
import User.Agent.Types (TestRunId (..), mkTestRunId)
import User.Types (Phase (..), TestRun, TestRunState, URL (..))

intro :: String
intro =
    [s|
    Cardano Antithesis Agent Process

    This process will run indefinitely, polling the recipient email for results,
    and changing the relative test-run facts to published when results are found.

    To stop the process, simply interrupt it (Ctrl+C).

    To get help on the available options, use the --help flag.

    To get bash cli completion use

    > source <(anti-agent --bash-completion-script "$(which anti-agent)")

    Fish and zsh completions are also available.
    |]

parseArgs :: IO ProcessOptions
parseArgs =
    runParser
        version
        intro
        $ withYamlConfig
            secretsFileOption
            processOptionsParser

data ProcessOptions = ProcessOptions
    { poAuth :: Auth
    , poPollIntervalSeconds :: Int
    , poWallet :: Wallet
    , poTokenId :: TokenId
    , poMPFSClient :: MPFSClient
    , poAntithesisEmail :: EmailUser
    , poAntithesisEmailPassword :: EmailPassword
    , poDays :: Int
    }

processOptionsParser :: Parser ProcessOptions
processOptionsParser =
    ProcessOptions
        <$> githubAuthOption
        <*> pollIntervalOption
        <*> walletOption
        <*> tokenIdOption
        <*> mpfsClientOption
        <*> agentEmailOption
        <*> agentEmailPasswordOption
        <*> daysOption

agentProcess
    :: ProcessOptions
    -> IO ()
agentProcess opts@ProcessOptions{poPollIntervalSeconds} = do
    putStrLn "Starting agent process service..."
    forever $ do
        results <- pollEmails opts
        putStrLn
            $ "Found " ++ show (length results) ++ " email results."
        (runningTests, doneTests) <- pollRunningTests opts
        putStrLn
            $ "Found "
                ++ show (length runningTests)
                ++ " running tests and "
                ++ show (length doneTests)
                ++ " done tests."
        for_ results $ \result@Result{description} -> do
            let sameKey :: [Fact TestRun v] -> Maybe (Fact TestRun v)
                sameKey = find $ (description ==) . factKey
                matchingTests =
                    Left <$> sameKey runningTests
                        <|> Right <$> sameKey doneTests
                TestRunId trId = mkTestRunId description
            for_ matchingTests $ \case
                Left (Fact testRun testState) -> do
                    putStrLn
                        $ "Publishing result for test-run: "
                            ++ show testRun
                    eres <- submit opts (testRunDuration testState) result
                    case eres of
                        ValidationFailure err ->
                            putStrLn
                                $ "Failed to publish result for test-run "
                                    ++ trId
                                    ++ ": "
                                    ++ show err
                        ValidationSuccess txHash ->
                            putStrLn
                                $ "Published result for test-run "
                                    ++ trId
                                    ++ " in transaction "
                                    ++ show txHash
                Right _ -> do
                    putStrLn
                        $ "Test-run "
                            ++ trId
                            ++ " is already in done state."
        putStrLn
            $ "Sleeping for "
                ++ show poPollIntervalSeconds
                ++ " seconds..."
        threadDelay (poPollIntervalSeconds * 1000000)

pollEmails :: ProcessOptions -> IO [Result]
pollEmails
    ProcessOptions
        { poMPFSClient
        , poAuth
        , poAntithesisEmail
        , poAntithesisEmailPassword
        , poDays
        } = do
        eresults <-
            cmd
                $ AgentCommand
                    poAuth
                    poMPFSClient
                $ CheckAllResults
                    poAntithesisEmail
                    poAntithesisEmailPassword
                    poDays

        case eresults of
            ValidationFailure err -> error $ "Failed to get email results: " ++ show err
            ValidationSuccess results -> pure results

pollRunningTests
    :: ProcessOptions
    -> IO
        ( [Fact TestRun (TestRunState 'RunningT)]
        , [Fact TestRun (TestRunState 'DoneT)]
        )
pollRunningTests
    ProcessOptions
        { poMPFSClient
        , poTokenId
        } = do
        allTrs <-
            cmd
                $ GetFacts Nothing poMPFSClient poTokenId
                $ TestRunFacts (AnyTestRuns [] All)
        let typed :: FromJSON Maybe x => [Fact TestRun x]
            typed = mapMaybe (mapM fromJSON) allTrs
        pure (typed, typed)

submit
    :: ProcessOptions
    -> Duration
    -> Result
    -> IO
        ( AValidationResult
            ReportFailure
            (WithTxHash (TestRunState DoneT))
        )
submit
    ProcessOptions{poAuth, poMPFSClient, poWallet, poTokenId}
    duration
    Result{description, link} =
        cmd
            $ AgentCommand poAuth poMPFSClient
            $ Report
                poTokenId
                poWallet
                (mkTestRunId description)
                ()
                duration
            $ URL
            $ T.unpack link

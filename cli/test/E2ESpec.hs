module E2ESpec
    ( spec
    ) where

import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec (Spec, describe, it)

runScenario :: FilePath -> IO ()
runScenario script = do
    let scriptFile = "test/scenarios/" ++ script
    (code, stdout, sterr) <- readProcessWithExitCode scriptFile [] ""
    putStrLn stdout
    case code of
        ExitSuccess -> return ()
        _ ->
            error $ "Scenario failed: " ++ scriptFile ++ "\n" ++ sterr

spec :: Spec
spec = do
    describe "E2E tests" $ do
        describe "should prove that agent" $ do
            it "can reject a test-run"
                $ runScenario "rejectATestRun.sh"
            it "can accept a test-run"
                $ runScenario "acceptATestRun.sh"
            it "can finish a test-run"
                $ runScenario "finishATestRun.sh"
            it "can validate a test-run"
                $ runScenario "validateATestRun.sh"

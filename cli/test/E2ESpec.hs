module E2ESpec
    ( spec
    ) where

import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec (Spec, describe, it)

runScenario :: FilePath -> IO ()
runScenario script = do
    let scriptFile = "test/scenarios/" ++ script
    (code, _, sterr) <- readProcessWithExitCode scriptFile [] ""
    case code of
        ExitSuccess -> return ()
        _ ->
            error $ "Scenario failed: " ++ scriptFile ++ "\n" ++ sterr

spec :: Spec
spec = do
    describe "E2E tests" $ do
        describe "should prove that agent" $ do
            it "can to reject a test-run"
                $ runScenario "rejectATestRun.sh"
            it "can to accept a test-run"
                $ runScenario "acceptATestRun.sh"
            it "can finish a test-run"
                $ runScenario "finishATestRun.sh"

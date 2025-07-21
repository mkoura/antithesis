module E2ESpec
    ( spec
    ) where

import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec (Spec, describe, it, xit)

runScenario :: FilePath -> IO ()
runScenario script = do
    let scriptFile = "test/scenarios/" ++ script
    (code, _stdout, sterr) <- readProcessWithExitCode scriptFile [] ""
    case code of
        ExitSuccess -> return ()
        _ ->
            error $ "Scenario failed: " ++ scriptFile ++ "\n" ++ sterr

spec :: Spec
spec = do
    describe "E2E tests" $ do
        describe "should prove that the system" $ do
            it "can run a real world scenario"
                $ runScenario "realWorld.sh"
            xit "can validate a user registration and his role"
                $ runScenario "validateUserRegAddRole.sh"

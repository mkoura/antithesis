module E2ESpec
    ( e2eSpec
    ) where

import GitHub (Auth)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec
    ( ActionWith
    , SpecWith
    , aroundAllWith
    , describe
    , it
    )

import qualified Debug.Trace as TR

runScenario :: FilePath -> IO ()
runScenario script = do
    let scriptFile = "test-E2E/scenarios/" ++ script
    (code, stdout, sterr) <- readProcessWithExitCode scriptFile [] ""
    TR.trace ("code:"<>show code <>"\nstdout:"<> show stdout<>"\nsterr:"<>show sterr) $ case code of
        ExitSuccess ->
            return ()
        _ ->
            error $ "Scenario failed: " ++ scriptFile ++ "\n" ++ sterr

setup :: ActionWith () -> ActionWith Auth
setup action _auth = do
    action ()
    return ()

e2eSpec :: SpecWith Auth
e2eSpec = do
    aroundAllWith setup $ do
        describe "End-to-End Tests" $ do
            it "should run the real world scenario" $ do
                runScenario "realWorld.sh"
            it "should retract a request" $ do
                runScenario "retractions.sh"
            it "should validate a user registration and his role" $ do
                runScenario "validateUserRegAddRole.sh"

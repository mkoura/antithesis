module Oracle.Validate.Requests.DownloadAssets
    ( renderDownloadAssetsFailure
    , validateDownloadAssets
    , DownloadAssetsFailure (..)
    )
where

import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO (..))
import Core.Types.Basic (Directory, Platform (..), Repository)
import Core.Types.Fact
    ( Fact (..)
    , keyHash
    )
import Lib.GitHub (GithubResponseStatusCodeError)
import Text.JSON.Canonical (ToJSON (..))
import Lib.JSON.Canonical.Extra (object, (.=))
import Oracle.Validate.Types
    ( Validate
    , Validated (..)
    , throwFalse
    )
import User.Agent.Types
    ( TestRunId (..)
    , TestRunMap (..)
    , TestRunStatus (..)
    )
import User.Types (Phase (..), TestRun (..))
import Validation
    ( Validation (..)
    )

data DownloadAssetsFailure
    = DownloadAssetsPlatformUnsupported Platform
    | DownloadAssetsRepositoryNotInThePlatform Repository
    | DownloadAssetsTestRunIdNotFound TestRunId
    | DownloadAssetsSourceDirFailure SourceDirFailure
    deriving (Show, Eq)

renderDownloadAssetsFailure :: DownloadAssetsFailure -> String
renderDownloadAssetsFailure (DownloadAssetsPlatformUnsupported platform) =
    "Platform is missing: " ++ show platform
renderDownloadAssetsFailure (DownloadAssetsRepositoryNotInThePlatform repo) =
    "Repository is not in the platform: " ++ show repo
renderDownloadAssetsFailure (DownloadAssetsTestRunIdNotFound (TestRunId testid)) =
    "Requested test id : " ++ show testid ++ " not found. Please refer to created ones using 'anti agent query'"

instance Monad m => ToJSON m DownloadAssetsFailure where
    toJSON (DownloadAssetsPlatformUnsupported platform) =
        object ["error" .= ("Platform is missing: " ++ show platform)]
    toJSON (DownloadAssetsRepositoryNotInThePlatform repo) =
        object
            ["error" .= ("Repository is not in the platform: " ++ show repo)]
    toJSON (DownloadAssetsTestRunIdNotFound (TestRunId testid)) =
        object ["error" .= ("Requested test id : " ++ show testid ++ " not found. Please refer to created ones using 'anti agent query'")]

data SourceDirFailure =
      SourceDirFailureDirAbsent
    | SourceDirFailureGithubError GithubResponseStatusCodeError
    deriving (Show, Eq)

checkDirectory
    :: MonadIO m
    => Validation m
    -> TestRun
    -> m (Maybe SourceDirFailure)
checkDirectory
    Validation{githubDirectoryExists}
    testRun = do
        existsE <-
            githubDirectoryExists
                (repository testRun)
                (commitId testRun)
                (directory testRun)
        pure $ case existsE of
            Left err -> Just $ SourceDirFailureGithubError err
            Right exists ->
                if exists
                    then Nothing
                    else Just SourceDirFailureDirAbsent

validateDownloadAssets
    :: Monad m
    => Validation m
    -> TestRunMap
    -> TestRunId
    -> Directory
    -> Validate DownloadAssetsFailure m Validated
validateDownloadAssets _v TestRunMap{pending,running,done} testid _dir = do
    pendingR <- filterM inspectTestRunPending pending
    runningR <- filterM inspectTestRunRunning running
    doneR <- filterM inspectTestRunDone done
    let matched =
            (extractTestRunPending <$> pendingR) ++
            (extractTestRunRunning <$> runningR) ++
            (extractTestRunDone <$> doneR)
    throwFalse (null matched)
        $ DownloadAssetsTestRunIdNotFound testid
  where
    checkFact testrun  = do
        keyId <- keyHash @_ @TestRun testrun
        pure $ testid == TestRunId keyId
    extractTestRunPending :: TestRunStatus PendingT -> TestRun
    extractTestRunPending (StatusPending fact) = factKey fact
    extractTestRunRunning :: TestRunStatus RunningT -> TestRun
    extractTestRunRunning (StatusRunning fact) = factKey fact
    extractTestRunDone :: TestRunStatus DoneT -> TestRun
    extractTestRunDone (StatusDone fact) = factKey fact
    inspectTestRunPending (StatusPending fact) = checkFact $ factKey fact
    inspectTestRunRunning (StatusRunning fact) = checkFact $ factKey fact
    inspectTestRunDone (StatusDone fact) = checkFact $ factKey fact

module Oracle.Validate.Requests.DownloadAssets
    ( renderDownloadAssetsFailure
    , validateDownloadAssets
    , DownloadAssetsFailure (..)
    )
where

import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic (Directory(..), Platform (..), Repository)
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
    , mapFailure
    , notValidated
    , throwFalse
    , throwJusts
    )
import System.Directory (doesDirectoryExist, getPermissions, writable)
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
    | DownloadAssetsTargetDirNotFound
    | DownloadAssetsTargetDirNotWritable
    deriving (Show, Eq)

renderDownloadAssetsFailure :: DownloadAssetsFailure -> String
renderDownloadAssetsFailure (DownloadAssetsPlatformUnsupported platform) =
    "Platform is missing: " ++ show platform
renderDownloadAssetsFailure (DownloadAssetsRepositoryNotInThePlatform repo) =
    "Repository is not in the platform: " ++ show repo
renderDownloadAssetsFailure (DownloadAssetsTestRunIdNotFound (TestRunId testid)) =
    "Requested test id : " ++ show testid ++ " not found. Please refer to created ones using 'anti agent query'"
renderDownloadAssetsFailure (DownloadAssetsSourceDirFailure SourceDirFailureDirAbsent) =
    "There is no directory specified by test run"
renderDownloadAssetsFailure (DownloadAssetsSourceDirFailure (SourceDirFailureGithubError err)) =
    "When checking directory specified by test run github responded with " ++ show err
renderDownloadAssetsFailure DownloadAssetsTargetDirNotFound =
    "There is no target local directory"
renderDownloadAssetsFailure DownloadAssetsTargetDirNotWritable =
    "The target local directory is not writable"

instance Monad m => ToJSON m DownloadAssetsFailure where
    toJSON (DownloadAssetsPlatformUnsupported platform) =
        object ["error" .= ("Platform is missing: " ++ show platform)]
    toJSON (DownloadAssetsRepositoryNotInThePlatform repo) =
        object
            ["error" .= ("Repository is not in the platform: " ++ show repo)]
    toJSON (DownloadAssetsTestRunIdNotFound (TestRunId testid)) =
        object ["error" .= ("Requested test id : " ++ show testid ++ " not found. Please refer to created ones using 'anti agent query'")]
    toJSON (DownloadAssetsSourceDirFailure SourceDirFailureDirAbsent) =
        object ["error" .= ("There is no directory specified by test run" :: String)]
    toJSON (DownloadAssetsSourceDirFailure (SourceDirFailureGithubError err)) =
        object ["error" .= ("When checking directory specified by test run github responded with " ++ show err)]
    toJSON DownloadAssetsTargetDirNotFound =
        object ["error" .= ("There is no target local directory" :: String)]
    toJSON DownloadAssetsTargetDirNotWritable =
        object ["error" .= ("The target local directory is not writable" :: String)]

data SourceDirFailure =
      SourceDirFailureDirAbsent
    | SourceDirFailureGithubError GithubResponseStatusCodeError
    deriving (Show, Eq)

checkSourceDirectory
    :: Monad m
    => Validation m
    -> TestRun
    -> m (Maybe SourceDirFailure)
checkSourceDirectory
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

checkTargetDirectory
    :: Directory
    -> IO Bool
checkTargetDirectory (Directory dir) =
    doesDirectoryExist dir

checkTargetDirectoryPermissions
    :: Directory
    -> IO Bool
checkTargetDirectoryPermissions (Directory dir) =
    writable <$> getPermissions dir

validateDownloadAssets
    :: MonadIO m
    => Validation m
    -> TestRunMap
    -> TestRunId
    -> Directory
    -> Validate DownloadAssetsFailure m Validated
validateDownloadAssets validation TestRunMap{pending,running,done} testid dir = do
    pendingR <- filterM inspectTestRunPending pending
    runningR <- filterM inspectTestRunRunning running
    doneR <- filterM inspectTestRunDone done
    let matched =
            (extractTestRunPending <$> pendingR) ++
            (extractTestRunRunning <$> runningR) ++
            (extractTestRunDone <$> doneR)
    case matched of
        [] -> notValidated $  DownloadAssetsTestRunIdNotFound testid
        firstMatched : _ -> do
            sourceDirValidation <- lift $ checkSourceDirectory validation firstMatched
            _ <- mapFailure DownloadAssetsSourceDirFailure $ throwJusts sourceDirValidation
            targetDirValidation <- liftIO $ checkTargetDirectory dir
            _ <- throwFalse targetDirValidation DownloadAssetsTargetDirNotFound
            targetDirWritableValidation <- liftIO $ checkTargetDirectoryPermissions dir
            throwFalse targetDirWritableValidation DownloadAssetsTargetDirNotWritable
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

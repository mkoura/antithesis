module Oracle.Validate.Requests.DownloadAssets
    ( renderDownloadAssetsFailure
    , validateDownloadAssets
    , DownloadAssetsFailure (..)
    )
where

import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic (Directory(..), FileName(..))
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
import System.Directory (doesDirectoryExist, getPermissions, withCurrentDirectory, writable)
import User.Agent.Types
    ( TestRunId (..)
    , TestRunMap (..)
    , TestRunStatus (..)
    )
import User.Types (Phase (..), TestRun (..))
import Validation
    ( Validation (..)
    )
import Validation.DownloadFile
    ( DownloadedFileFailure (..)
    , renderDownloadedFileFailure
    )

import qualified Data.Text.IO as T

data DownloadAssetsFailure
    = DownloadAssetsTestRunIdNotFound TestRunId
    | DownloadAssetsSourceDirFailure SourceDirFailure
    | DownloadAssetsTargetDirNotFound
    | DownloadAssetsTargetDirNotWritable
    | DownloadAssetsValidationError DownloadedFileFailure
    deriving (Show, Eq)

renderDownloadAssetsFailure :: DownloadAssetsFailure -> String
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
renderDownloadAssetsFailure (DownloadAssetsValidationError failure) =
    "The file cannot pass validation. Details:  " <> renderDownloadedFileFailure failure

instance Monad m => ToJSON m DownloadAssetsFailure where
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
    toJSON (DownloadAssetsValidationError failure) =
        object ["error" .= ("The file cannot pass validation. Details:  " <> renderDownloadedFileFailure failure)]

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

downloadFileAndWriteLocally
    :: MonadIO m
    => Validation m
    -> TestRun
    -> Directory
    -> FileName
    -> m (Maybe DownloadAssetsFailure)
downloadFileAndWriteLocally
    Validation{githubGetFile}
    testRun
    (Directory targetDir)
    (FileName filename) = do
        let (Directory sourceDir) = directory testRun
        contentE <-
            githubGetFile
                (repository testRun)
                (commitId testRun)
                (FileName $ sourceDir <> "/" <> filename)
        case contentE of
            Left err -> pure $ Just $ DownloadAssetsValidationError err
            Right txt -> do
                liftIO $ withCurrentDirectory targetDir $
                    T.writeFile filename txt
                pure Nothing

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
validateDownloadAssets validation TestRunMap{pending,running,done} testid dir@(Directory dir') = do
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

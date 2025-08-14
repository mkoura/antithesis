module Oracle.Validate.Requests.DownloadAssets
    ( renderDownloadAssetsFailure
    , validateDownloadAssets
    , DownloadAssetsFailure (..)
    )
where

import Core.Types.Basic (Directory, Platform (..), Repository)
import Core.Types.Fact
    ( Fact (..)
    , keyHash
    )
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
import Validation
    ( Validation (..)
    )

data DownloadAssetsFailure
    = DownloadAssetsPlatformUnsupported Platform
    | DownloadAssetsRepositoryNotInThePlatform Repository
    | DownloadAssetsTestRunIdNotFound TestRunId
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

validateDownloadAssets
    :: Monad m
    => Validation m
    -> TestRunMap
    -> TestRunId
    -> Directory
    -> Validate DownloadAssetsFailure m Validated
validateDownloadAssets _v TestRunMap{pending,running,done} testid _dir = do
    pendingR <- mapM inspectTestRunPending pending
    runningR <- mapM inspectTestRunRunning running
    doneR <- mapM inspectTestRunDone done
    let matched = foldl (||) False $ pendingR ++ runningR ++ doneR
    throwFalse matched
        $ DownloadAssetsTestRunIdNotFound testid
  where
    checkFact fact  = do
        keyId <- keyHash fact
        pure $ testid == TestRunId keyId
    inspectTestRunPending (StatusPending fact) = checkFact $ factKey fact
    inspectTestRunRunning (StatusRunning fact) = checkFact $ factKey fact
    inspectTestRunDone (StatusDone fact) = checkFact $ factKey fact

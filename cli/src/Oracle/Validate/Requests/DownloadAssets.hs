module Oracle.Validate.Requests.DownloadAssets
    ( renderDownloadAssetsFailure
    , DownloadAssetsFailure (..)
    )
where

import Core.Types.Basic (Platform (..), Repository)
import Text.JSON.Canonical (ToJSON (..))
import Lib.JSON.Canonical.Extra (object, (.=))

data DownloadAssetsFailure
    = DownloadAssetsPlatformUnsupported Platform
    | DownloadAssetsRepositoryNotInThePlatform Repository
    deriving (Show, Eq)

renderDownloadAssetsFailure :: DownloadAssetsFailure -> String
renderDownloadAssetsFailure (DownloadAssetsPlatformUnsupported platform) =
    "Platform is missing: " ++ show platform
renderDownloadAssetsFailure (DownloadAssetsRepositoryNotInThePlatform repo) =
    "Repository is not in the platform: " ++ show repo

instance Monad m => ToJSON m DownloadAssetsFailure where
    toJSON (DownloadAssetsPlatformUnsupported platform) =
        object ["error" .= ("Platform is missing: " ++ show platform)]
    toJSON (DownloadAssetsRepositoryNotInThePlatform repo) =
        object
            ["error" .= ("Repository is not in the platform: " ++ show repo)]

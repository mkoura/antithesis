module Oracle.Validate.Requests.ManageWhiteList
    ( validateAddWhiteListed
    , validateRemoveWhiteListed
    , UpdateWhiteListFailure (..)
    )
where

import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans (..))
import Core.Types.Basic (Platform (..), Repository)
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Operation (Op (..))
import Lib.GitHub qualified as Github
import Lib.JSON.Canonical.Extra (object, (.=))
import Oracle.Validate.Types
    ( Validate
    , Validated (..)
    , mapFailure
    , throwFalse
    , throwLeft
    )
import Text.JSON.Canonical (ToJSON (..))
import User.Agent.Types (WhiteListKey (..))
import Validation
    ( KeyFailure
    , Validation (..)
    , deleteValidation
    , insertValidation
    )

data UpdateWhiteListFailure
    = WhiteListPlatformUnsupported Platform
    | WhiteListRepositoryNotInThePlatform Repository
    | WhiteListRepositoryKeyValidation KeyFailure
    | WhiteListGithubFailed Github.GithubResponseStatusCodeError
    deriving (Show, Eq)

instance Monad m => ToJSON m UpdateWhiteListFailure where
    toJSON (WhiteListPlatformUnsupported platform) =
        object ["error" .= ("Platform is missing: " ++ show platform)]
    toJSON (WhiteListRepositoryNotInThePlatform repo) =
        object
            ["error" .= ("Repository is not in the platform: " ++ show repo)]
    toJSON (WhiteListRepositoryKeyValidation keyFailure) =
        object
            ["error" .= ("Repository key validation failed: " ++ show keyFailure)]
    toJSON (WhiteListGithubFailed err) =
        object ["error" .= ("GitHub error: " ++ show err)]

validateAddWhiteListed
    :: Monad m
    => Validation m
    -> Change WhiteListKey (OpI ())
    -> Validate UpdateWhiteListFailure m Validated
validateAddWhiteListed
    v@Validation{githubRepositoryExists}
    c@(Change (Key (WhiteListKey platform repo)) _) = do
        mapFailure WhiteListRepositoryKeyValidation $ insertValidation v c
        void
            $ throwFalse (platform == Platform "github")
            $ WhiteListPlatformUnsupported platform
        eexists <- lift $ githubRepositoryExists repo
        exist <- throwLeft WhiteListGithubFailed eexists
        throwFalse exist
            $ WhiteListRepositoryNotInThePlatform repo

validateRemoveWhiteListed
    :: Monad m
    => Validation m
    -> Change WhiteListKey (OpD ())
    -> Validate UpdateWhiteListFailure m Validated
validateRemoveWhiteListed v c@(Change (Key (WhiteListKey platform _repo)) _) = do
    mapFailure WhiteListRepositoryKeyValidation $ deleteValidation v c
    throwFalse (platform == Platform "github")
        $ WhiteListPlatformUnsupported platform

module Oracle.Validate.Requests.RegisterRole
    ( validateRegisterRole
    , validateUnregisterRole
    , RegisterRoleFailure (..)
    , UnregisterRoleFailure (..)
    , renderRegisterRoleFailure
    , renderUnregisterRoleFailure
    ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic
    ( Platform (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Operation
    ( Op (..)
    )
import Lib.JSON.Canonical.Extra (object, (.=))
import Oracle.Validate.Types
    ( Validate
    , Validated (..)
    , mapFailure
    , notValidated
    )
import Text.JSON.Canonical (ToJSON (..))
import User.Types
    ( RegisterRoleKey (..)
    )
import Validation
    ( KeyFailure
    , Validation (..)
    , deleteValidation
    , insertValidation
    , renderKeyFailure
    )
import Validation.RegisterRole
    ( RepositoryRoleFailure
    , renderRepositoryRoleFailure
    )

data RegisterRoleFailure
    = RoleNotPresentOnPlatform RepositoryRoleFailure
    | RegisterRolePlatformNotSupported String
    | RegisterRoleKeyFailure KeyFailure
    deriving (Eq, Show)

instance Monad m => ToJSON m RegisterRoleFailure where
    toJSON = \case
        RoleNotPresentOnPlatform reason ->
            object
                ["roleNotPresentOnPlatform" .= renderRepositoryRoleFailure reason]
        RegisterRolePlatformNotSupported platform ->
            object ["registerRolePlatformNotSupported" .= platform]
        RegisterRoleKeyFailure keyFailure ->
            object ["registerRoleKeyFailure" .= renderKeyFailure keyFailure]

renderRegisterRoleFailure :: RegisterRoleFailure -> String
renderRegisterRoleFailure = \case
    RoleNotPresentOnPlatform reason ->
        "Role not present on platform: " ++ renderRepositoryRoleFailure reason
    RegisterRolePlatformNotSupported platform ->
        "RegisterRole platform not supported: " ++ platform
    RegisterRoleKeyFailure keyFailure ->
        "RegisterRole key failure: " ++ renderKeyFailure keyFailure

validateRegisterRole
    :: Monad m
    => Validation m
    -> Change RegisterRoleKey (OpI ())
    -> Validate RegisterRoleFailure m Validated
validateRegisterRole
    validation@Validation{githubRepositoryRole}
    change@(Change (Key k) _) = do
        mapFailure RegisterRoleKeyFailure $ insertValidation validation change
        let RegisterRoleKey (Platform platform) repository username = k
        when (platform /= "github")
            $ notValidated
            $ RegisterRolePlatformNotSupported platform
        validationRes <- lift $ githubRepositoryRole username repository
        case validationRes of
            Just failure -> notValidated $ RoleNotPresentOnPlatform failure
            Nothing -> pure Validated

data UnregisterRoleFailure
    = UnregisterRoleKeyFailure KeyFailure
    | RoleIsPresentOnPlatform -- issue 1b6d49bb5fc6b7e4fcd8ab22436294a118451cb3
    deriving (Show, Eq)

instance Monad m => ToJSON m UnregisterRoleFailure where
    toJSON = \case
        UnregisterRoleKeyFailure keyFailure ->
            object ["unregisterRoleKeyFailure" .= renderKeyFailure keyFailure]
        RoleIsPresentOnPlatform ->
            object ["roleIsPresentOnPlatform" .= ()]

renderUnregisterRoleFailure :: UnregisterRoleFailure -> String
renderUnregisterRoleFailure = \case
    UnregisterRoleKeyFailure keyFailure ->
        "UnregisterRole key failure: " ++ renderKeyFailure keyFailure
    RoleIsPresentOnPlatform ->
        "Role is present on platform, cannot unregister role."

validateUnregisterRole
    :: Monad m
    => Validation m
    -> Change RegisterRoleKey (OpD ())
    -> Validate UnregisterRoleFailure m Validated
validateUnregisterRole
    validation
    change@(Change (Key _k) _) = do
        mapFailure UnregisterRoleKeyFailure
            $ deleteValidation validation change

-- issue 1b6d49bb5fc6b7e4fcd8ab22436294a118451cb3

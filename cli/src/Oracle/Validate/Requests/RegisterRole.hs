module Oracle.Validate.Requests.RegisterRole
    ( validateRegisterRole
    , validateUnregisterRole
    , RegisterRoleFailure (..)
    , UnregisterRoleFailure (..)
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
    )
import Validation.RegisterRole
    ( RepositoryRoleFailure
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
                ["roleNotPresentOnPlatform" .= reason]
        RegisterRolePlatformNotSupported platform ->
            object ["registerRolePlatformNotSupported" .= platform]
        RegisterRoleKeyFailure keyFailure ->
            object ["registerRoleKeyFailure" .= keyFailure]

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
            object ["unregisterRoleKeyFailure" .= keyFailure]
        RoleIsPresentOnPlatform ->
            object ["roleIsPresentOnPlatform" .= ()]

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
        pure Validated

-- issue 1b6d49bb5fc6b7e4fcd8ab22436294a118451cb3

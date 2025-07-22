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
    ( Username (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..))
import Core.Types.Operation
    ( Op (..)
    )
import Data.List (find)
import Oracle.Validate.Types
    ( ValidationResult
    , mapFailure
    , notValidated
    , runValidate
    )
import User.Types
    ( RegisterRoleKey (..)
    , RegisterUserKey (..)
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
    | RegisterRoleUserNotRegistered Username
    deriving (Eq, Show)

renderRegisterRoleFailure :: RegisterRoleFailure -> String
renderRegisterRoleFailure = \case
    RoleNotPresentOnPlatform reason ->
        "Role not present on platform: " ++ renderRepositoryRoleFailure reason
    RegisterRolePlatformNotSupported platform ->
        "RegisterRole platform not supported: " ++ platform
    RegisterRoleKeyFailure keyFailure ->
        "RegisterRole key failure: " ++ renderKeyFailure keyFailure
    RegisterRoleUserNotRegistered (Username user) ->
        "RegisterRole user not registered: " ++ user

validateRegisterRole
    :: Monad m
    => Validation m
    -> Change RegisterRoleKey (OpI ())
    -> m (ValidationResult RegisterRoleFailure)
validateRegisterRole
    validation@Validation{mpfsGetFacts, githubRepositoryRole}
    change@(Change (Key k) _) = runValidate $ do
        mapFailure RegisterRoleKeyFailure $ insertValidation validation change
        facts <- lift mpfsGetFacts
        let RegisterRoleKey platform repository username = k
            registration = flip find facts
                $ \(Fact (RegisterUserKey platform' username' _) ()) ->
                    platform' == platform && username' == username
        when (null registration)
            $ notValidated
            $ RegisterRoleUserNotRegistered username
        validationRes <- lift $ githubRepositoryRole username repository
        case validationRes of
            Just failure -> notValidated $ RoleNotPresentOnPlatform failure
            Nothing -> pure ()

data UnregisterRoleFailure
    = UnregisterRolePlatformNotSupported String
    | UnregisterRoleKeyFailure KeyFailure
    | RoleIsPresentOnPlatform -- issue 1b6d49bb5fc6b7e4fcd8ab22436294a118451cb3
    deriving (Show, Eq)

renderUnregisterRoleFailure :: UnregisterRoleFailure -> String
renderUnregisterRoleFailure = \case
    UnregisterRolePlatformNotSupported platform ->
        "UnregisterRole platform not supported: " ++ platform
    UnregisterRoleKeyFailure keyFailure ->
        "UnregisterRole key failure: " ++ renderKeyFailure keyFailure
    RoleIsPresentOnPlatform ->
        "Role is present on platform, cannot unregister role."

validateUnregisterRole
    :: Monad m
    => Validation m
    -> Change RegisterRoleKey (OpD ())
    -> m (ValidationResult UnregisterRoleFailure)
validateUnregisterRole
    validation
    change@(Change (Key _k) _) = runValidate $ do
        mapFailure UnregisterRoleKeyFailure
            $ deleteValidation validation change

-- issue 1b6d49bb5fc6b7e4fcd8ab22436294a118451cb3

{-# LANGUAGE OverloadedRecordDot #-}

module Oracle.Validate.Requests.RegisterRole
    ( validateRegisterRole
    , validateUnregisterRole
    , RegisterRoleFailure (..)
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
    )
import Validation.RegisterRole (RepositoryRoleFailure)

data RegisterRoleFailure
    = RoleNotPresentOnPlatform RepositoryRoleFailure
    | RegisterRolePlatformNotSupported String
    | RegisterRoleKeyFailure KeyFailure
    | RegisterRoleUserNotRegistered Username
    deriving (Eq, Show)

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

validateUnregisterRole
    :: Monad m
    => Validation m
    -> Change RegisterRoleKey (OpD ())
    -> m (ValidationResult String)
validateUnregisterRole
    validation@Validation{mpfsGetFacts}
    change@(Change (Key k) _) = runValidate $ do
        mapFailure show $ deleteValidation validation change
        facts <- lift mpfsGetFacts
        let registration = find (\(Fact k' ()) -> k' == k) facts
        when (null registration)
            $ notValidated
            $ "no registration of the 'antithesis' role for '"
                <> show k.platform
                <> "' platform and '"
                <> show k.repository
                <> "' repository found"

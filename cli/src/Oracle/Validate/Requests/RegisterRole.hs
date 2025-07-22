{-# LANGUAGE OverloadedRecordDot #-}

module Oracle.Validate.Requests.RegisterRole
    ( validateRegisterRole
    , validateUnregisterRole
    ) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic
    ( Platform (..)
    , Repository (..)
    , Username (..)
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
    ( Validation (..)
    , deleteValidation
    , insertValidation
    )
import Validation.RegisterRole qualified as Github

validateRegisterRole
    :: Monad m
    => Validation m
    -> Change RegisterRoleKey (OpI ())
    -> m (ValidationResult String)
validateRegisterRole
    validation@Validation{mpfsGetFacts, githubRepositoryRole}
    change@(Change (Key k) _) = runValidate $ do
        mapFailure show $ insertValidation validation change
        facts <- lift mpfsGetFacts
        let RegisterRoleKey platform repository username = k
            registration = flip find facts
                $ \(Fact (RegisterUserKey platform' username' _) ()) ->
                    platform' == platform && username' == username
        if null registration
            then
                let Platform p = platform
                    Repository o r = repository
                    Username u = username
                in  notValidated
                        $ "no registration for platform '"
                            <> show p
                            <> "' and repository '"
                            <> show r
                            <> "' of owner '"
                            <> show o
                            <> "' and user '"
                            <> show u
                            <> "' found"
            else do
                validationRes <- lift $ githubRepositoryRole username repository
                unless (validationRes == Github.RepoRoleValidated)
                    $ notValidated
                    $ Github.emitRepoRoleMsg validationRes

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

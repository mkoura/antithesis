{-# LANGUAGE OverloadedRecordDot #-}

module Oracle.Validate.Requests.RegisterRole
    ( validateRegisterRole
    , validateUnregisterRole
    ) where

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
import Oracle.Validate.Types (ValidationResult (..))
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
    -> m ValidationResult
validateRegisterRole
    validation@Validation{mpfsGetFacts, githubRepositoryRole}
    change@(Change (Key k) _) = do
        insertionValidation <- insertValidation validation change
        if insertionValidation /= Validated
            then pure insertionValidation
            else do
                facts <- mpfsGetFacts
                let RegisterRoleKey platform repository username = k
                    registration = flip find facts
                        $ \(Fact (RegisterUserKey platform' username' _) ()) ->
                            platform' == platform && username' == username
                if null registration
                    then
                        let Platform p = platform
                            Repository o r = repository
                            Username u = username
                        in  pure
                                $ NotValidated
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
                        validationRes <- githubRepositoryRole username repository
                        pure
                            $ if validationRes == Github.RepoRoleValidated
                                then
                                    Validated
                                else
                                    NotValidated
                                        $ Github.emitRepoRoleMsg validationRes

validateUnregisterRole
    :: Monad m
    => Validation m
    -> Change RegisterRoleKey (OpD ())
    -> m ValidationResult
validateUnregisterRole
    validation@Validation{mpfsGetFacts}
    change@(Change (Key k) _) = do
        deletionValidation <- deleteValidation validation change
        if deletionValidation /= Validated
            then pure deletionValidation
            else do
                facts <- mpfsGetFacts
                let registration = find (\(Fact k' ()) -> k' == k) facts
                pure
                    $ if null registration
                        then
                            NotValidated
                                $ "no registration of the 'antithesis' role for '"
                                    <> show k.platform
                                    <> "' platform and '"
                                    <> show k.repository
                                    <> "' repository found"
                        else
                            Validated

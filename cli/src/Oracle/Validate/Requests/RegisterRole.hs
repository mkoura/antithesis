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
import Lib.Github.GetRepoRole qualified as Github
import Oracle.Validate.Types (ValidationResult (..))
import Servant.Client (ClientM)
import User.Types
    ( RegisterRoleKey (..)
    , RegisterUserKey (..)
    )
import Validation (Validation (..))

validateRegisterRole
    :: Validation ClientM
    -> Change RegisterRoleKey (OpI ())
    -> ClientM ValidationResult
validateRegisterRole
    Validation{mpfsGetFacts, githubRepositoryRole}
    (Change k _v) = do
        facts <- mpfsGetFacts
        let Key
                ( RegisterRoleKey
                        platform
                        repository
                        username
                    ) = k
        let registration = flip find facts
                $ \(Fact (RegisterUserKey platform' username' _) ()) ->
                    platform' == platform
                        && username' == username

        if null registration
            then
                let (Platform p) = platform
                    (Repository o r) = repository
                    (Username u) = username
                in  pure
                        ( NotValidated
                            $ "no registration for platform '"
                                <> show p
                                <> "' and repository '"
                                <> show r
                                <> "' of owner '"
                                <> show o
                                <> "' and user '"
                                <> show u
                                <> "' found"
                        )
            else do
                validationRes <-
                    githubRepositoryRole
                        username
                        repository
                if validationRes == Github.RepoRoleValidated
                    then
                        pure Validated
                    else
                        pure $ NotValidated (Github.emitRepoRoleMsg validationRes)

validateUnregisterRole
    :: Validation ClientM
    -> Change RegisterRoleKey (OpD ())
    -> ClientM ValidationResult
validateUnregisterRole
    Validation{mpfsGetFacts}
    (Change (Key k) _v) = do
        facts <- mpfsGetFacts
        let registration = find (\(Fact k' ()) -> k' == k) facts
        if null registration
            then
                pure
                    $ NotValidated
                    $ "no registration of the 'antithesis' role for '"
                        <> show k.platform
                        <> "' platform and '"
                        <> show k.repository
                        <> "' repository found"
            else
                pure Validated

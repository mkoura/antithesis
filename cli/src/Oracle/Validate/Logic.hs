{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Logic
    ( ValidationResult (..)
    , validateRequest
    ) where

import Core.Types.Basic
    ( Owner
    , Platform (..)
    , Repository (..)
    , RequestRefId
    , Username (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..))
import Data.List (find)
import Lib.Github.GetRepoRole qualified as Github
import Oracle.Types (Request (..), RequestZoo (..))
import Oracle.Validate.Requests.RegisterUser
    ( validateRegisterUser
    , validateUnregisterUser
    )
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig
    )
import Oracle.Validate.Requests.TestRun.Create (validateCreateTestRun)
import Oracle.Validate.Requests.TestRun.Others
    ( validateToDoneUpdate
    , validateToRunningUpdate
    )
import Oracle.Validate.Types (ValidationResult (..))
import Servant.Client (ClientM)
import User.Types
    ( RegisterRoleKey (..)
    , RegisterUserKey (..)
    )
import Validation (Validation (..))

validateRequest
    :: TestRunValidationConfig
    -> Owner
    -> Validation ClientM
    -> RequestZoo
    -> ClientM (RequestRefId, ValidationResult)
validateRequest
    _
    _
    validation
    (RegisterUserRequest (Request refId _owner change)) =
        (,) refId <$> validateRegisterUser validation change
validateRequest
    _
    _
    validation
    (UnregisterUserRequest (Request refId _owner change)) =
        (,) refId <$> validateUnregisterUser validation change
validateRequest
    _
    _
    Validation{mpfsGetFacts, githubRepositoryRole}
    (RegisterRoleRequest (Request refId _owner (Change k _v))) = do
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
                        ( refId
                        , NotValidated
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
                        pure (refId, Validated)
                    else
                        pure (refId, NotValidated (Github.emitRepoRoleMsg validationRes))
validateRequest
    _
    _
    Validation{mpfsGetFacts}
    (UnregisterRoleRequest (Request refId _owner (Change (Key k) _v))) = do
        facts <- mpfsGetFacts
        let registration = find (\(Fact k' ()) -> k' == k) facts
        if null registration
            then
                pure
                    ( refId
                    , NotValidated
                        $ "no registration of the 'antithesis' role for '"
                            <> show k.platform
                            <> "' platform and '"
                            <> show k.repository
                            <> "' repository found"
                    )
            else
                pure (refId, Validated)
validateRequest testRunConfig _ validation (CreateTestRequest rq) =
    (,) (outputRefId rq)
        <$> validateCreateTestRun testRunConfig validation (change rq)
validateRequest _ antiOwner validation (RejectRequest rq) =
    (,) (outputRefId rq) <$> validateToDoneUpdate antiOwner validation rq
validateRequest _ antiOwner validation (AcceptRequest rq) =
    (,) (outputRefId rq)
        <$> validateToRunningUpdate antiOwner validation rq
validateRequest _ antiOwner validation (FinishedRequest rq) =
    (,) (outputRefId rq) <$> validateToDoneUpdate antiOwner validation rq

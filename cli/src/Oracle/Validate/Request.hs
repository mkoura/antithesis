module Oracle.Validate.Request
    ( ValidationResult (..)
    , validateRequest
    ) where

import Core.Types.Basic
    ( Owner
    , RequestRefId
    )
import Oracle.Types (Request (..), RequestZoo (..))
import Oracle.Validate.Requests.RegisterRole
    ( validateRegisterRole
    , validateUnregisterRole
    )
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
    validation
    (RegisterRoleRequest (Request refId _owner change)) =
        (,) refId <$> validateRegisterRole validation change
validateRequest
    _
    _
    validation
    (UnregisterRoleRequest (Request refId _owner change)) =
        (,) refId <$> validateUnregisterRole validation change
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

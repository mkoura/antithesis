module Oracle.Validate.Request
    ( validateRequest
    ) where

import Core.Types.Basic
    ( Owner
    , RequestRefId
    )
import Oracle.Types
    ( Request (..)
    , RequestValidationFailure (..)
    , RequestZoo (..)
    )
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
import Oracle.Validate.Types (ValidationResult, withValidationResult)
import Servant.Client (ClientM)
import Validation (Validation (..))

validateRequest
    :: TestRunValidationConfig
    -> Owner
    -> Validation ClientM
    -> RequestZoo
    -> ClientM (RequestRefId, ValidationResult RequestValidationFailure)
validateRequest _ _ validation (RegisterUserRequest (Request refId _ change)) =
    (,) refId . withValidationResult RegisterUserFailure
        <$> validateRegisterUser validation change
validateRequest _ _ validation (UnregisterUserRequest (Request refId _ change)) =
    (,) refId . withValidationResult UnregisterUserFailure
        <$> validateUnregisterUser validation change
validateRequest _ _ validation (RegisterRoleRequest (Request refId _ change)) =
    (,) refId . withValidationResult RegisterRoleFailure
        <$> validateRegisterRole validation change
validateRequest _ _ validation (UnregisterRoleRequest (Request refId _ change)) =
    (,) refId . withValidationResult GenericFailure
        <$> validateUnregisterRole validation change
validateRequest testRunConfig _ validation (CreateTestRequest (Request refId _ change)) =
    (,) refId . withValidationResult GenericFailure
        <$> validateCreateTestRun testRunConfig validation change
validateRequest _ antiOwner validation (RejectRequest (Request refId owner change)) =
    (,) refId . withValidationResult GenericFailure
        <$> validateToDoneUpdate antiOwner validation owner change
validateRequest _ antiOwner validation (AcceptRequest (Request refId owner change)) =
    (,) refId . withValidationResult GenericFailure
        <$> validateToRunningUpdate antiOwner validation owner change
validateRequest _ antiOwner validation (FinishedRequest (Request refId owner change)) =
    (,) refId . withValidationResult GenericFailure
        <$> validateToDoneUpdate antiOwner validation owner change

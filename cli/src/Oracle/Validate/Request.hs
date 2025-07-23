module Oracle.Validate.Request
    ( validateRequest
    ) where

import Core.Types.Basic
    ( Owner
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
import Oracle.Validate.Requests.TestRun.Update
    ( validateToDoneUpdate
    , validateToRunningUpdate
    )
import Oracle.Validate.Types
    ( Validate
    , Validated
    , mapFailure
    )
import Validation (Validation (..))

validateRequest
    :: Monad m
    => TestRunValidationConfig
    -> Owner
    -> Validation m
    -> RequestZoo
    -> Validate RequestValidationFailure m Validated
validateRequest _ _ validation (RegisterUserRequest (Request _ _ change)) =
    mapFailure RegisterUserFailure
        $ validateRegisterUser validation change
validateRequest _ _ validation (UnregisterUserRequest (Request _ _ change)) =
    mapFailure UnregisterUserFailure
        $ validateUnregisterUser validation change
validateRequest _ _ validation (RegisterRoleRequest (Request _ _ change)) =
    mapFailure RegisterRoleFailure
        $ validateRegisterRole validation change
validateRequest _ _ validation (UnregisterRoleRequest (Request _ _ change)) =
    mapFailure UnregisterRoleFailure
        $ validateUnregisterRole validation change
validateRequest testRunConfig _ validation (CreateTestRequest (Request _ _ change)) =
    mapFailure CreateTestRunFailure
        $ validateCreateTestRun testRunConfig validation change
validateRequest _ antiOwner validation (RejectRequest (Request _ owner change)) =
    mapFailure UpdateTestRunFailure
        $ validateToDoneUpdate antiOwner validation owner change
validateRequest _ antiOwner validation (AcceptRequest (Request _ owner change)) =
    mapFailure UpdateTestRunFailure
        $ validateToRunningUpdate antiOwner validation owner change
validateRequest _ antiOwner validation (FinishedRequest (Request _ owner change)) =
    mapFailure UpdateTestRunFailure
        $ validateToDoneUpdate antiOwner validation owner change

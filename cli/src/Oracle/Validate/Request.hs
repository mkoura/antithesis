module Oracle.Validate.Request
    ( validateRequest
    ) where

import Control.Monad.IO.Class (MonadIO (..))
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
    :: MonadIO m
    => TestRunValidationConfig
    -> Owner
    -- ^ agent pkh
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
validateRequest _ agentPKH validation (RejectRequest (Request _ requester change)) =
    mapFailure UpdateTestRunFailure
        $ validateToDoneUpdate validation agentPKH requester change
validateRequest _ agentPKH validation (AcceptRequest (Request _ requester change)) =
    mapFailure UpdateTestRunFailure
        $ validateToRunningUpdate validation agentPKH requester change
validateRequest _ agentPKH validation (FinishedRequest (Request _ requester change)) =
    mapFailure UpdateTestRunFailure
        $ validateToDoneUpdate validation agentPKH requester change

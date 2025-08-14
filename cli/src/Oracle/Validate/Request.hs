module Oracle.Validate.Request
    ( validateRequest
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Oracle.Config.Types (Config (..))
import Oracle.Types
    ( Request (..)
    , RequestValidationFailure (..)
    , RequestZoo (..)
    )
import Oracle.Validate.Requests.ManageWhiteList
    ( validateAddWhiteListed
    , validateRemoveWhiteListed
    )
import Oracle.Validate.Requests.RegisterRole
    ( validateRegisterRole
    , validateUnregisterRole
    )
import Oracle.Validate.Requests.RegisterUser
    ( validateRegisterUser
    , validateUnregisterUser
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
    , notValidated
    )
import Validation (Validation (..))

validateRequest
    :: MonadIO m
    => Config
    -> Validation m
    -> RequestZoo
    -> Validate RequestValidationFailure m Validated
validateRequest _ validation (RegisterUserRequest (Request _ _ change)) =
    mapFailure RegisterUserFailure
        $ validateRegisterUser validation change
validateRequest _ validation (UnregisterUserRequest (Request _ _ change)) =
    mapFailure UnregisterUserFailure
        $ validateUnregisterUser validation change
validateRequest _ validation (RegisterRoleRequest (Request _ _ change)) =
    mapFailure RegisterRoleFailure
        $ validateRegisterRole validation change
validateRequest _ validation (UnregisterRoleRequest (Request _ _ change)) =
    mapFailure UnregisterRoleFailure
        $ validateUnregisterRole validation change
validateRequest Config{configTestRun} validation (CreateTestRequest (Request _ _ change)) =
    mapFailure CreateTestRunFailure
        $ validateCreateTestRun configTestRun validation change
validateRequest Config{configAgent} validation (RejectRequest (Request _ requester change)) =
    mapFailure UpdateTestRunFailure
        $ validateToDoneUpdate validation configAgent requester change
validateRequest Config{configAgent} validation (AcceptRequest (Request _ requester change)) =
    mapFailure UpdateTestRunFailure
        $ validateToRunningUpdate validation configAgent requester change
validateRequest Config{configAgent} validation (FinishedRequest (Request _ requester change)) =
    mapFailure UpdateTestRunFailure
        $ validateToDoneUpdate validation configAgent requester change
validateRequest Config{configAgent} validation (WhiteListRequest (Request _ requester change)) =
    mapFailure WhiteListFailure
        $ validateAddWhiteListed validation configAgent requester change
validateRequest Config{configAgent} validation (BlackListRequest (Request _ requester change)) =
    mapFailure WhiteListFailure
        $ validateRemoveWhiteListed validation configAgent requester change
validateRequest _ _ (UnknownInsertRequest request) =
    notValidated
        $ UnknownInsertValidationFailure request
validateRequest _ _ (UnknownDeleteRequest request) =
    notValidated
        $ UnknownDeleteValidationFailure request
validateRequest _ _ (UnknownUpdateRequest request) =
    notValidated
        $ UnknownUpdateValidationFailure request

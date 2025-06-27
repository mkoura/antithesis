{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Logic
    ( ValidationResult (..)
    , toJSONValidationResult
    , validateRequest
    ) where

import Core.Types (RequestRefId)
import Lib.JSON
    ( stringJSON
    )
import Oracle.Types (Request (..), RequestZoo (..))
import Text.JSON.Canonical
    ( JSValue (..)
    )

data ValidationResult
    = Validated
    | NotValidated
    | CannotValidate
    | NotEvaluated
    deriving (Eq, Show)

toJSONValidationResult :: Monad m => ValidationResult -> m JSValue
toJSONValidationResult Validated = stringJSON "validated"
toJSONValidationResult NotValidated = stringJSON "not validated"
toJSONValidationResult CannotValidate = stringJSON "cannot validate"
toJSONValidationResult NotEvaluated = stringJSON "not evaluated"

validateRequest
    :: Applicative m
    => RequestZoo
    -> m (RequestRefId, ValidationResult)
validateRequest (RegisterUserRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (UnregisterUserRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (RegisterRoleRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (UnregisterRoleRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)

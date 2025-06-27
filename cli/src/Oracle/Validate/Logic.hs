{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Logic
    ( validateMPFSRequestTemplate
    , validateMPFSRequest
    , ValidationResult (..)
    , toJSONValidationResult
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Core.Types (Key (..), PublicKeyHash, Username)
import Lib.JSON
    ( stringJSON
    )
import MPFS.Types (MPFSOperation (..), MPFSRequest)
import MPFS.Types qualified as MPFS
import Oracle.Github.ListPublicKeys
    ( PublicKeyValidation (..)
    , inspectPublicKey
    )
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ReportSchemaErrors (..)
    )
import User.Types (RegisterPublicKey (..), UnregisterPublicKey (..))

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

parseRegisterPubKey
    :: ReportSchemaErrors m => JSValue -> m RegisterPublicKey
parseRegisterPubKey = fromJSON @_ @RegisterPublicKey

parseUnregisterPubKey
    :: ReportSchemaErrors m => JSValue -> m UnregisterPublicKey
parseUnregisterPubKey = fromJSON @_ @UnregisterPublicKey

validateMPFSRequestTemplate
    :: (ReportSchemaErrors m)
    => MPFSRequest
    -> (Username -> PublicKeyHash -> m PublicKeyValidation)
    -> m ValidationResult
validateMPFSRequestTemplate request validatePubKey = do
    let (Key key) = MPFS.key $ MPFS.change request
        op = MPFS.operation $ MPFS.change request
    case op of
        InsertOp -> do
            RegisterPublicKey{username, pubkeyhash} <- parseRegisterPubKey key
            validationRes <- validatePubKey username pubkeyhash
            case validationRes of
                PublicKeyValidated ->
                    pure Validated
                _ ->
                    pure NotValidated
        DeleteOp -> do
            UnregisterPublicKey{username, pubkeyhash} <- parseUnregisterPubKey key
            validationRes <- validatePubKey username pubkeyhash
            case validationRes of
                PublicKeyValidated ->
                    pure Validated
                _ ->
                    pure NotValidated
        _ ->
            pure NotEvaluated

validateMPFSRequest
    :: (MonadIO m, ReportSchemaErrors m)
    => MPFSRequest
    -> m ValidationResult
validateMPFSRequest request =
    validateMPFSRequestTemplate request $ \u r -> liftIO $ inspectPublicKey u r

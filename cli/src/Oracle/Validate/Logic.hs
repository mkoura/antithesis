{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Logic
    ( validateMPFSRequestTemplate
    , validateMPFSRequest
    , ValidationResult (..)
    ) where

import Core.Types (Key (..), PublicKeyHash, Username)
import MPFS.Types (MPFSOperation (..), MPFSRequest)
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

import MPFS.Types qualified as MPFS

data ValidationResult
    = Validated
    | NotValidated
    | CannotValidate
    | NotEvaluated
    deriving (Eq, Show)

instance ReportSchemaErrors IO where
    expected expct (Just got) =
        fail
            $ "Expected: "
                ++ expct
                ++ ", but got: "
                ++ got
    expected expct Nothing = fail $ "Expected: " ++ expct

parseRegisterPubKey :: JSValue -> IO RegisterPublicKey
parseRegisterPubKey = fromJSON @_ @RegisterPublicKey

parseUnregisterPubKey :: JSValue -> IO UnregisterPublicKey
parseUnregisterPubKey = fromJSON @_ @UnregisterPublicKey

validateMPFSRequestTemplate
    :: MPFSRequest
    -> (Username -> PublicKeyHash -> IO PublicKeyValidation)
    -> IO ValidationResult
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
    :: MPFSRequest
    -> IO ValidationResult
validateMPFSRequest request =
    validateMPFSRequestTemplate request inspectPublicKey

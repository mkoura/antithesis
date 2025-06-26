{-# LANGUAGE StrictData #-}

module Oracle.Validate.Logic
    ( validateMPFSRequest
    , ValidationResult (..)
    ) where

import Core.Types (Key (..), OutputReference)
import Oracle.Types (MPFSRequest)
import Oracle.Types qualified as Oracle

data ValidationResult
    = Validated OutputReference
    | NotValidated
    | CannotValidate
    deriving (Eq, Show)

validateMPFSRequest
    :: MPFSRequest
    -> (Key -> IO ValidationResult)
    -> IO (OutputReference, ValidationResult)
validateMPFSRequest request validateKeyAction = do
    let key = Oracle.key . Oracle.change $ request
        ref = Oracle.outputRefId request
    validateRes <- validateKeyAction key
    pure (ref, validateRes)

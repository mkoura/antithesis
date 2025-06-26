{-# LANGUAGE StrictData #-}

module Oracle.Validate.Logic
    ( validateMPFSRequest
    , ValidationResult (..)
    ) where

import Network.HTTP.Client (Request)
import Servant.Client (ClientM)

data ValidationResult
    = Validated
    | NotValidated
    | CannotValidate
    | NotEvaluated
    deriving (Eq, Show)

validateMPFSRequest
    :: Request
    -> ClientM ValidationResult
validateMPFSRequest _request = pure NotEvaluated

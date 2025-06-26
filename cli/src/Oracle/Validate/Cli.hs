module Oracle.Validate.Cli
    ( validateCmd
    , ValidateCommand (..)
    ) where

import Core.Types
    ( TokenId
    )
import MPFS.API
    ( getToken
    )
import Servant.Client (ClientM)
import Text.JSON.Canonical (JSValue)

data ValidateCommand
    = ValidateRequests
    deriving (Eq, Show)

validateCmd :: TokenId -> ValidateCommand -> ClientM JSValue
validateCmd tk command = do
    case command of
        ValidateRequests -> do
            _canonicalJSON <- getToken tk
            undefined

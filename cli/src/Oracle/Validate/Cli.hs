module Oracle.Validate.Cli
    ( validateCmd
    , ValidateCommand (..)
    ) where

import Core.Types
    ( TokenId
    )
import Data.Aeson (Value)
import MPFS.API
    ( getToken
    )
import Servant.Client (ClientM)

data ValidateCommand
    = ValidateRequests
    deriving (Eq, Show)

validateCmd :: TokenId -> ValidateCommand -> ClientM Value
validateCmd tk command = do
    case command of
        ValidateRequests -> do
            canonicalJSON <- getToken tk
            undefined

{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Cli
    ( validateCmd
    , ValidateCommand (..)
    ) where

import Control.Monad.IO.Class
    ( liftIO
    )
import Core.Types
    ( TokenId
    )
import MPFS.API
    ( getToken
    )
import MPFS.Types
    ( MPFSGetToken
    , MPFSRequest (..)
    , MPFSRequestChange (..)
    , MPFSTokenState (..)
    )
import Servant.Client (ClientM)
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue
    , ReportSchemaErrors (..)
    )

import MPFS.Types qualified as MPFS

data ValidateCommand
    = ValidateRequests
    deriving (Eq, Show)

instance ReportSchemaErrors ClientM where
    expected expct (Just got) =
        liftIO
            $ fail
            $ "Expected: "
                ++ expct
                ++ ", but got: "
                ++ got
    expected expct Nothing = liftIO $ fail $ "Expected: " ++ expct

validateCmd :: TokenId -> ValidateCommand -> ClientM JSValue
validateCmd tk command = do
    case command of
        ValidateRequests -> do
            canonicalJSON <- getToken tk
            resp <- fromJSON @_ @MPFSGetToken canonicalJSON
            let requestsToValidate = MPFS.requests resp
            undefined

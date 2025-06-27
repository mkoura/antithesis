{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Cli
    ( validateCmd
    , ValidateCommand (..)
    ) where

import Control.Monad.IO.Class
    ( liftIO
    )
import Core.Types
    ( RequestRefId (..)
    , TokenId
    )
import Lib.JSON
    ( stringJSON
    )
import MPFS.API
    ( getToken
    )
import MPFS.Types
    ( MPFSGetToken
    , MPFSRequest (..)
    )
import Oracle.Validate.Logic
    ( toJSONValidationResult
    , validateMPFSRequest
    )
import Servant.Client (ClientM)
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ReportSchemaErrors (..)
    , mkObject
    , toJSString
    )

import Data.Text qualified as T
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
            JSArray <$> mapM constructRes (MPFS.requests resp)
  where
    constructRes request@MPFSRequest{requestOutput} = do
        validationRes <- validateMPFSRequest request
        createPairResJSValue (requestOutput, validationRes)

    createPairResJSValue (RequestRefId ref, validation) =
        mkObject
            [ (toJSString "reference", stringJSON $ T.unpack ref)
            , (toJSString "validationResult", toJSONValidationResult validation)
            ]

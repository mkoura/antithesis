{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Cli
    ( validateCmd
    , ValidateCommand (..)
    , Parsing (..)
    ) where

import Control.Monad (forM)
import Control.Monad.Trans.Class (lift)
import Core.Context
    ( WithContext
    , askMkValidation
    , askMpfs
    , askTestRunConfig
    , askWalletOwner
    )
import Core.Types.Basic (RequestRefId (RequestRefId), TokenId)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Lib.JSON
    ( Parsing (..)
    , stringJSON
    )
import MPFS.API
    ( MPFS (..)
    )
import Oracle.Types
    ( RequestValidationFailure
    , Token (..)
    , requestZooRefId
    )
import Oracle.Validate.Request
    ( validateRequest
    )
import Oracle.Validate.Types (ValidationResult, runValidate)
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ToJSON (..)
    , mkObject
    , toJSString
    )

data ValidateCommand
    = ValidateRequests
    deriving (Eq, Show)

validateCmd
    :: Monad m
    => TokenId
    -> ValidateCommand
    -> WithContext m JSValue
validateCmd tk command = do
    mpfs <- askMpfs
    testRunConfig <- askTestRunConfig
    pkh <- askWalletOwner
    validation <- askMkValidation tk
    rus <- lift $ case command of
        ValidateRequests -> do
            canonicalJSON <- mpfsGetToken mpfs tk
            requests <- do
                let mv = fromJSON canonicalJSON <&> tokenRequests
                case mv of
                    Nothing -> error "Failed to parse token"
                    Just jsValue -> pure jsValue
            forM requests $ \request -> do
                valid <-
                    runValidate
                        $ validateRequest testRunConfig pkh validation request
                mkResult (requestZooRefId request) valid
    toJSON rus

mkResult
    :: Monad m
    => RequestRefId
    -> ValidationResult RequestValidationFailure
    -> m JSValue
mkResult (RequestRefId ref) validation =
    mkObject
        [ (toJSString "reference", stringJSON $ T.unpack ref)
        , (toJSString "validation", toJSON validation)
        ]

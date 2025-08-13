{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Cli
    ( validateCmd
    , ValidateCommand (..)
    , RequestValidation (..)
    , OracleValidateFailure (..)
    , renderOracleValidateFailure
    ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Core.Context
    ( WithContext
    , askAgentPKH
    , askMpfs
    , askTestRunConfig
    , askValidation
    )
import Core.Types.Basic (RequestRefId, TokenId)
import Data.Functor ((<&>))
import Lib.JSON.Canonical.Extra
    ( object
    , (.=)
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
import Oracle.Validate.Types
    ( AValidationResult
    , ValidationResult
    , notValidated
    , runValidate
    )
import Text.JSON.Canonical
    ( FromJSON (..)
    , ToJSON (..)
    )

data ValidateCommand a where
    ValidateRequests
        :: ValidateCommand
            ( AValidationResult
                OracleValidateFailure
                [RequestValidation]
            )

deriving instance Show (ValidateCommand a)
deriving instance Eq (ValidateCommand a)

newtype OracleValidateFailure
    = OracleValidateFailToParseToken TokenId
    deriving (Show, Eq)

instance Monad m => ToJSON m OracleValidateFailure where
    toJSON (OracleValidateFailToParseToken tk) =
        object ["error" .= ("Failed to parse token with ID: " ++ show tk)]

renderOracleValidateFailure :: OracleValidateFailure -> String
renderOracleValidateFailure (OracleValidateFailToParseToken tk) =
    "Failed to parse token with ID: " ++ show tk

validateCmd
    :: MonadIO m
    => TokenId
    -> ValidateCommand a
    -> WithContext m a
validateCmd tk command = case command of
    ValidateRequests -> do
        mpfs <- askMpfs
        testRunConfig <- askTestRunConfig
        pkh <- askAgentPKH
        validation <- askValidation tk
        lift $ runValidate $ case command of
            ValidateRequests -> do
                canonicalJSON <- lift $ mpfsGetToken mpfs tk
                requests <- do
                    let mv = fromJSON canonicalJSON <&> tokenRequests
                    case mv of
                        Nothing ->
                            notValidated
                                $ OracleValidateFailToParseToken tk
                        Just jsValue -> pure jsValue
                forM requests $ \request -> lift $ do
                    RequestValidation (requestZooRefId request)
                        <$> runValidate
                            (validateRequest testRunConfig pkh validation request)

data RequestValidation = RequestValidation
    { requestRefId :: RequestRefId
    , validationResult :: ValidationResult RequestValidationFailure
    }
    deriving (Eq, Show)

instance Monad m => ToJSON m RequestValidation where
    toJSON (RequestValidation refId validation) =
        object
            [ "reference" .= refId
            , "validation" .= validation
            ]

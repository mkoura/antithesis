{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Cli
    ( validateCmd
    , ValidateCommand (..)
    , RequestValidation (..)
    , OracleValidateFailure (..)
    ) where

import Control.Monad (forM)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Core.Context
    ( WithContext
    , askConfig
    , askMpfs
    , askValidation
    )
import Core.Types.Basic (RequestRefId, TokenId)
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
    , TokenState (..)
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

data OracleValidateFailure
    = OracleValidateFailToParseToken TokenId
    | OracleValidateConfigNotAvailable
    deriving (Show, Eq)

instance Monad m => ToJSON m OracleValidateFailure where
    toJSON (OracleValidateFailToParseToken tk) =
        object ["error" .= ("Failed to parse token with ID: " ++ show tk)]
    toJSON OracleValidateConfigNotAvailable =
        toJSON ("Token configuration is not available yet" :: String)

validateCmd
    :: (MonadIO m, MonadMask m)
    => TokenId
    -> ValidateCommand a
    -> WithContext m a
validateCmd tk command = case command of
    ValidateRequests -> do
        mpfs <- askMpfs
        mconfig <- askConfig tk
        validation <- askValidation $ Just tk
        lift $ runValidate $ case command of
            ValidateRequests -> do
                canonicalJSON <- lift $ mpfsGetToken mpfs tk
                (oracle, requests) <- do
                    let mtoken = fromJSON canonicalJSON
                    case mtoken of
                        Nothing ->
                            notValidated
                                $ OracleValidateFailToParseToken tk
                        Just jsValue ->
                            pure
                                ( tokenOwner $ tokenState jsValue
                                , tokenRequests jsValue
                                )
                forM requests $ \request -> lift $ do
                    RequestValidation (requestZooRefId request)
                        <$> runValidate
                            (validateRequest oracle mconfig validation request)

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

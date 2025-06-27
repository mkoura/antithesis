{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Cli
    ( validateCmd
    , ValidateCommand (..)
    , Parsing (..)
    ) where

import Control.Monad (forM)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Core.Types
    ( RequestRefId (..)
    , TokenId
    )
import Data.Functor ((<&>))
import Data.Text qualified as T
import Lib.JSON
    ( Parsing (..)
    , stringJSON
    )
import MPFS.API
    ( getToken
    )
import Oracle.Types (Token (..))
import Oracle.Validate.Logic
    ( ValidationResult
    , toJSONValidationResult
    , validateRequest
    )
import Servant.Client (ClientM)
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

validateCmd :: TokenId -> ValidateCommand -> ClientM JSValue
validateCmd tk command = do
    rus <- case command of
        ValidateRequests -> do
            canonicalJSON <- getToken tk
            requests <- do
                v <-
                    runMaybeT
                        $ runParsing
                        $ fromJSON canonicalJSON
                        <&> tokenRequests
                case v of
                    Nothing -> error "Failed to parse token"
                    Just jsValue -> pure jsValue
            forM requests $ \request -> do
                validateRequest request >>= uncurry mkResult
    toJSON rus

mkResult :: Monad m => RequestRefId -> ValidationResult -> m JSValue
mkResult (RequestRefId ref) validation =
    mkObject
        [ (toJSString "reference", stringJSON $ T.unpack ref)
        , (toJSString "validation", toJSONValidationResult validation)
        ]

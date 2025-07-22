{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Cli
    ( validateCmd
    , ValidateCommand (..)
    , Parsing (..)
    ) where

import Control.Monad (forM)
import Control.Monad.Trans.Except (runExceptT)
import Core.Types.Basic (Owner, RequestRefId (..), TokenId)
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
import Oracle.Validate.Request
    ( validateRequest
    )
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig
    )
import Oracle.Validate.Types (ValidationResult)
import Servant.Client (ClientM)
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ToJSON (..)
    , mkObject
    , toJSString
    )
import Validation (Validation)

data ValidateCommand
    = ValidateRequests
    deriving (Eq, Show)

validateCmd
    :: TestRunValidationConfig
    -> Owner
    -> Validation ClientM
    -> TokenId
    -> ValidateCommand
    -> ClientM JSValue
validateCmd testRunConfig pkh validation tk command = do
    rus <- case command of
        ValidateRequests -> do
            canonicalJSON <- getToken tk
            requests <- do
                v <-
                    runExceptT
                        $ runParsing
                        $ fromJSON canonicalJSON
                        <&> tokenRequests
                case v of
                    Left e -> error $ "Failed to parse token: " ++ e
                    Right jsValue -> pure jsValue
            forM requests $ \request -> do
                validateRequest testRunConfig pkh validation request
                    >>= uncurry mkResult
    toJSON rus

mkResult
    :: Monad m => RequestRefId -> ValidationResult String -> m JSValue
mkResult (RequestRefId ref) validation =
    mkObject
        [ (toJSString "reference", stringJSON $ T.unpack ref)
        , (toJSString "validation", toJSON validation)
        ]

module Oracle.Validate.Types
    ( ValidationResult (..)
    ) where

import Lib.JSON
    ( stringJSON
    )
import Text.JSON.Canonical.Class (ToJSON (..))

data ValidationResult
    = Validated
    | NotValidated String
    | CannotValidate String
    | NotEvaluated
    deriving (Eq, Show)

instance Monad m => ToJSON m ValidationResult where
    toJSON = \case
        Validated -> stringJSON "validated"
        NotValidated reason -> stringJSON $ "not validated: " <> reason
        CannotValidate reason -> stringJSON $ "cannot validate: " <> reason
        NotEvaluated -> stringJSON "not evaluated"

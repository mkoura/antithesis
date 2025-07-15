{-# LANGUAGE DeriveGeneric #-}

module Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data TestRunValidationConfig = TestRunValidationConfig
    { maxDuration :: Int
    , minDuration :: Int
    }
    deriving (Show, Eq, Generic)

instance ToJSON TestRunValidationConfig
instance FromJSON TestRunValidationConfig

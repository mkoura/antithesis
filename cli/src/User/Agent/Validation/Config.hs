{-# LANGUAGE DeriveGeneric #-}

module User.Agent.Validation.Config
    ( AgentValidationConfig (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data AgentValidationConfig = AgentValidationConfig
    { maxDuration :: Int
    , minDuration :: Int
    }
    deriving (Show, Eq, Generic)

instance ToJSON AgentValidationConfig
instance FromJSON AgentValidationConfig

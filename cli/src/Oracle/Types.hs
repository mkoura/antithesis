{-# LANGUAGE StrictData #-}

module Oracle.Types
    ( RequestRefs (..)
    ) where

import Core.Types (OutputReference)
import Data.Aeson
    ( FromJSON (parseJSON)
    , KeyValue ((.=))
    , ToJSON (toJSON)
    , object
    , withObject
    , (.:)
    )

newtype RequestRefs = RequestRefs
    { outputReferences :: [OutputReference]
    }
    deriving (Eq, Show)

instance ToJSON RequestRefs where
    toJSON (RequestRefs{outputReferences}) =
        object
            [ "requestIds" .= outputReferences
            ]
instance FromJSON RequestRefs where
    parseJSON = withObject "Requests" $ \v ->
        RequestRefs
            <$> v .: "requestIds"

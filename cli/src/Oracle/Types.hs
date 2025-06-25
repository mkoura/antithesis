{-# LANGUAGE StrictData #-}

module Oracle.Types
    ( RequestRefs (..)
    , MPFSRequest (..)
    ) where

import Core.Types (Key, Operation, OutputReference, Owner)
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

data RequestChange = RequestChange
    { owner :: Owner
    , key :: Key
    , value :: Operation
    }
    deriving (Eq, Show)


data MPFSRequest = MPFSRequest
    { outputRefId :: OutputReference
    , change :: RequestChange
    , owner :: Owner
    }
    deriving (Eq, Show)

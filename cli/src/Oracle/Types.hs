{-# LANGUAGE StrictData #-}

module Oracle.Types
    ( Request (..)
    ) where

import Core.Types (Key, Operation, Owner, RequestRefId)
import Lib.JSON
import Text.JSON.Canonical

data Request k v = Request
    { ref :: RequestRefId
    , owner :: Owner
    , key :: Key k
    , operation :: Operation v
    }
    deriving (Show, Eq)

instance
    (Monad m, ToJSON m (Key k), ToJSON m (Operation v))
    => ToJSON m (Request k v)
    where
    toJSON (Request ref owner key operation) =
        object
            [ "ref" .= ref
            , "owner" .= owner
            , "key" .= key
            , "operation" .= operation
            ]

instance
    (ReportSchemaErrors m, FromJSON m (Key k), FromJSON m (Operation v))
    => FromJSON m (Request k v)
    where
    fromJSON = withObject "Request" $ \v ->
        Request
            <$> v .: "ref"
            <*> v .: "owner"
            <*> v .: "key"
            <*> v .: "operation"
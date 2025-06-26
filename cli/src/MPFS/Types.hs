{-# LANGUAGE StrictData #-}

module MPFS.Types
    ( MPFSRequest (..)
    , MPFSRequestChange (..)
    , MPFSTokenState (..)
    , MPFSGetToken (..)
    , MPFSOperation (..)
    ) where

import Core.Types
    ( Key (..)
    , Owner (..)
    , RequestRefId (..)
    , Root (..)
    , Val (..)
    )
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Lib.JSON
    ( getField
    , getListField
    , getStringField
    , object
    , stringJSON
    )
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ReportSchemaErrors
    , ToJSON (..)
    , expectedButGotValue
    , fromJSString
    , toJSString
    )

data MPFSRequestChange = MPFSRequestChange
    { key :: Key
    , value :: Val
    , operation :: MPFSOperation
    }
    deriving (Eq, Show)

data MPFSRequest = MPFSRequest
    { requestOutput :: RequestRefId
    , change :: MPFSRequestChange
    , owner :: Owner
    }
    deriving (Eq, Show)

data MPFSTokenState = MPFSTokenState
    { owner :: Owner
    , root :: Root
    }
    deriving (Eq, Show)

data MPFSGetToken = MPFSGetToken
    { outputRefId :: RequestRefId
    , state :: MPFSTokenState
    , requests :: [MPFSRequest]
    }
    deriving (Eq, Show)

data MPFSOperation
    = InsertOp
    | DeleteOp
    | UpdateOp
    deriving (Eq, Show)

toJSONOperation :: MPFSOperation -> JSValue
toJSONOperation InsertOp = JSString $ toJSString "insert"
toJSONOperation DeleteOp = JSString $ toJSString "delete"
toJSONOperation UpdateOp = JSString $ toJSString "update"

fromJSONOperation
    :: (ReportSchemaErrors m) => JSValue -> m MPFSOperation
fromJSONOperation (JSString jsString) = do
    let op = fromJSString jsString
    case op of
        "insert" -> pure InsertOp
        "delete" -> pure DeleteOp
        "update" -> pure UpdateOp
        _ -> expectedButGotValue "a valid operation" (JSString jsString)
fromJSONOperation other =
    expectedButGotValue "a string representing an operation" other

instance Monad m => ToJSON m MPFSRequestChange where
    toJSON
        ( MPFSRequestChange
                (Key key)
                (Val val)
                op
            ) =
            toJSON
                $ Map.fromList
                    [ ("key" :: String, key)
                    , ("value", val)
                    , ("operation", toJSONOperation op)
                    ]

instance (Monad m, ReportSchemaErrors m) => FromJSON m MPFSRequestChange where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        key <- getField "key" mapping
        val <- getField "value" mapping
        op <- fromJSONOperation <$> getField "operation" mapping
        MPFSRequestChange (Key key) (Val val) <$> op
    fromJSON r =
        expectedButGotValue
            "an object representing a MPFS request change"
            r

instance Monad m => ToJSON m MPFSRequest where
    toJSON
        ( MPFSRequest
                (RequestRefId ref)
                change
                (Owner owner)
            ) =
            object
                [ ("outputRefId", stringJSON $ T.unpack ref)
                , ("change", toJSON change)
                , ("owner", stringJSON owner)
                ]

instance (Monad m, ReportSchemaErrors m) => FromJSON m MPFSRequest where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        ref <- getStringField "outputRefId" mapping
        owner <- getStringField "owner" mapping
        change <- getField "change" mapping >>= fromJSON
        pure
            $ MPFSRequest
                { requestOutput = RequestRefId $ T.pack ref
                , change = change
                , owner = Owner owner
                }
    fromJSON r =
        expectedButGotValue
            "an object representing a MPFS request"
            r

instance Monad m => ToJSON m MPFSTokenState where
    toJSON
        ( MPFSTokenState
                (Owner owner)
                (Root root)
            ) =
            toJSON
                $ Map.fromList
                    [ ("owner", JSString $ toJSString owner)
                    , ("root" :: String, JSString $ toJSString root)
                    ]

instance (Monad m, ReportSchemaErrors m) => FromJSON m MPFSTokenState where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        owner <- getStringField "owner" mapping
        root <- getStringField "root" mapping
        pure
            $ MPFSTokenState
                { owner = Owner owner
                , root = Root root
                }
    fromJSON r =
        expectedButGotValue
            "an object representing token state"
            r

instance Monad m => ToJSON m MPFSGetToken where
    toJSON
        ( MPFSGetToken
                (RequestRefId ref)
                state
                requests
            ) =
            object
                [ ("outputRefId", stringJSON $ T.unpack ref)
                , ("state", toJSON state)
                , ("requests", traverse toJSON requests >>= toJSON)
                ]

instance (Monad m, ReportSchemaErrors m) => FromJSON m MPFSGetToken where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        ref <- getStringField "outputRefId" mapping
        state <- getField "state" mapping >>= fromJSON
        requests <- getListField "requests" mapping
        requestList <- traverse fromJSON requests
        pure
            $ MPFSGetToken
                { outputRefId = RequestRefId $ T.pack ref
                , state = state
                , requests = requestList
                }
    fromJSON r =
        expectedButGotValue
            "an object representing a MPFS get token response"
            r

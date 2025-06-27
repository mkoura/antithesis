{-# LANGUAGE StrictData #-}

module MPFS.Types
    ( MPFSTokenState (..)
    , MPFSGetToken (..)
    ) where

import Core.Types
    ( Owner (..)
    , RequestRefId (..)
    , Root (..)
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
    , toJSString
    )
import Oracle.Types (Request)

data MPFSTokenState = MPFSTokenState
    { owner :: Owner
    , root :: Root
    }
    deriving (Eq, Show)

data MPFSGetToken = MPFSGetToken
    { outputRefId :: RequestRefId
    , state :: MPFSTokenState
    , requests :: [Request JSValue JSValue]
    }
    deriving (Eq, Show)

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

instance ReportSchemaErrors m => ToJSON m MPFSGetToken where
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

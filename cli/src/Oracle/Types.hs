{-# LANGUAGE StrictData #-}

module Oracle.Types
    ( Request (..)
    , Token (..)
    , TokenState (..)
    , RequestZoo (..)
    ) where

import Core.Types (Change, Owner, RequestRefId, Root)
import Lib.JSON
import Text.JSON.Canonical
import User.Types (RegisterPublicKey, RegisterRoleKey)

data Request k v = Request
    { outputRefId :: RequestRefId
    , owner :: Owner
    , change :: Change k v
    }
    deriving (Show, Eq)

instance (Monad m, ToJSON m k, ToJSON m v) => ToJSON m (Request k v) where
    toJSON (Request refId owner change) =
        object
            [ "outputRefId" .= refId
            , "owner" .= owner
            , "change" .= change
            ]

instance
    (ReportSchemaErrors m, FromJSON m k, FromJSON m v)
    => FromJSON m (Request k v)
    where
    fromJSON = withObject "Request" $ \v -> do
        refId <- v .: "outputRefId"
        owner <- v .: "owner"
        change <- v .: "change"
        pure
            $ Request
                { outputRefId = refId
                , owner = owner
                , change = change
                }

data TokenState = TokenState
    { tokenRoot :: Root
    , tokenOwner :: Owner
    }

instance Monad m => ToJSON m TokenState where
    toJSON (TokenState root owner) =
        object
            [ "root" .= root
            , "owner" .= owner
            ]

instance ReportSchemaErrors m => FromJSON m TokenState where
    fromJSON = withObject "TokenState" $ \v -> do
        root <- v .: "root"
        owner <- v .: "owner"
        pure $ TokenState{tokenRoot = root, tokenOwner = owner}

data RequestZoo where
    RegisterUserRequest :: Request RegisterPublicKey String -> RequestZoo
    UnregisterUserRequest
        :: Request RegisterPublicKey String -> RequestZoo
    RegisterRoleRequest
        :: Request RegisterRoleKey String -> RequestZoo
    UnregisterRoleRequest
        :: Request RegisterRoleKey String -> RequestZoo

instance (ReportSchemaErrors m) => FromJSON m RequestZoo where
    fromJSON = withObject "RequestZoo" $ \v -> do
        requestType <- v .: "type"
        case requestType of
            JSString "register-user" -> do
                req <- v .: "request"
                pure $ RegisterUserRequest req
            JSString "unregister-user" -> do
                req <- v .: "request"
                pure $ UnregisterUserRequest req
            _ -> expectedButGotValue "RequestZoo" requestType

instance Monad m => ToJSON m RequestZoo where
    toJSON (RegisterUserRequest req) =
        object
            [ "type" .= ("register-user" :: String)
            , "request" .= req
            ]
    toJSON (UnregisterUserRequest req) =
        object
            [ "type" .= ("unregister-user" :: String)
            , "request" .= req
            ]
    toJSON (RegisterRoleRequest req) =
        object
            [ "type" .= ("register-role" :: String)
            , "request" .= req
            ]
    toJSON (UnregisterRoleRequest req) =
        object
            [ "type" .= ("unregister-role" :: String)
            , "request" .= req
            ]

data Token = Token
    { tokenRefId :: RequestRefId
    , tokenState :: TokenState
    , tokenRequests :: [RequestZoo]
    }

instance Monad m => ToJSON m Token where
    toJSON (Token refId state requests) =
        object
            [ "outputRefId" .= refId
            , "state" .= state
            , "requests" .= requests
            ]

instance
    ReportSchemaErrors m
    => FromJSON m Token
    where
    fromJSON = withObject "Token" $ \v -> do
        refId <- v .: "outputRefId"
        state <- v .: "state"
        requests <- v .: "requests"
        pure
            $ Token
                { tokenRefId = refId
                , tokenState = state
                , tokenRequests = requests
                }

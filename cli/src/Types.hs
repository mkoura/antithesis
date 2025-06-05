{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Types
    ( Command (..)
    , UserCommand (..)
    , RequesterCommand (..)
    , OracleCommand (..)
    , TokenCommand (..)
    , Directory (..)
    , Host (..)
    , Operation (..)
    , Options (..)
    , OutputReference (..)
    , Platform (..)
    , Port (..)
    , PublicKeyHash (..)
    , Repository (Repository)
    , Request (..)
    , Role (..)
    , SHA1 (..)
    , TokenId (..)
    , Username (..)
    ) where

import Data.Aeson
    ( FromJSON (parseJSON)
    , KeyValue ((.=))
    , ToJSON (toJSON)
    , Value (String)
    , object
    , withObject
    , withText
    , (.:)
    )
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Text.Read (readMaybe)

import qualified Data.Text as T

newtype TokenId = TokenId String
    deriving (Eq, Show)

instance ToHttpApiData TokenId where
    toUrlPiece (TokenId tokenId) = T.pack tokenId

instance FromHttpApiData TokenId where
    parseUrlPiece tokenId =
        case T.unpack tokenId of
            "" -> Left "TokenId cannot be empty"
            _ -> Right (TokenId (T.unpack tokenId))

newtype Platform = Platform String
    deriving (Eq, Show)

newtype PublicKeyHash = PublicKeyHash String
    deriving (Eq, Show)

newtype SHA1 = SHA1 String
    deriving (Eq, Show)

newtype TxId = TxId String
    deriving (Eq, Show)

newtype Role = Role String
    deriving (Eq, Show)

newtype Directory = Directory String
    deriving (Eq, Show)

newtype Username = Username String
    deriving (Eq, Show)

data Repository = Repository
    { organization :: String
    , project :: String
    }
    deriving (Eq, Show)

data Request = Request
    { key :: String
    , value :: String
    , operation :: Operation
    }
    deriving (Eq, Show)

instance ToJSON Request where
    toJSON (Request{key, value, operation}) =
        object
            [ "key" .= key
            , "value" .= value
            , "operation" .= operation
            ]

instance FromJSON Request where
    parseJSON = withObject "Request" $ \v ->
        Request
            <$> v .: "key"
            <*> v .: "value"
            <*> v .: "operation"

data Operation = Insert | Delete
    deriving (Eq, Show)

instance ToJSON Operation where
    toJSON Insert = String "insert"
    toJSON Delete = String "delete"

instance FromJSON Operation where
    parseJSON = withText "Operation" $ \v ->
        case v of
            "insert" -> pure Insert
            "delete" -> pure Delete
            _ -> fail $ "Invalid operation: " ++ T.unpack v

data OutputReference = OutputReference
    { outputReferenceTx :: String
    , outputReferenceIndex :: Int
    }
    deriving (Eq, Show)

instance ToJSON OutputReference where
    toJSON (OutputReference{outputReferenceTx, outputReferenceIndex}) =
        String
            $ T.pack outputReferenceTx
                <> "-"
                <> T.pack (show outputReferenceIndex)

instance FromJSON OutputReference where
    parseJSON = withText "OutputReference" $ \v -> do
        let parts = T.splitOn "-" v
        case parts of
            [tx, index] -> do
                case readMaybe (T.unpack index) of
                    Just i ->
                        pure
                            $ OutputReference
                                { outputReferenceTx = T.unpack tx
                                , outputReferenceIndex = i
                                }
                    Nothing -> fail $ "Invalid index: " ++ T.unpack index
            _ -> fail $ "Invalid output reference format: " ++ T.unpack v

newtype Port = Port Int
    deriving (Eq, Show)

newtype Host = Host String
    deriving (Eq, Show)

data Options = Options
    { host :: Host
    , port :: Port
    , command :: Command
    }
    deriving (Eq, Show)

data Command
    = UserCommand UserCommand
    | OracleCommand OracleCommand
    deriving (Eq, Show)

newtype OracleCommand
    = OracleTokenCommand TokenCommand
    deriving (Eq, Show)

data TokenCommand
    = CreateToken
    | DeleteToken
        { tokenId :: TokenId
        }
    | GetToken
        { tokenId :: TokenId
        }
    | UpdateToken
        { tokenId :: TokenId
        , requests :: [OutputReference]
        }
    deriving (Eq, Show)

newtype UserCommand
    = UserRequesterCommand RequesterCommand
    deriving (Eq, Show)

data RequesterCommand
    = RegisterPublicKey
        { platform :: Platform
        , username :: Username
        , pubkeyhash :: PublicKeyHash
        , tokenId :: TokenId
        }
    | UnregisterPublicKey
        { platform :: Platform
        , username :: Username
        , pubkeyhash :: PublicKeyHash
        , tokenId :: TokenId
        }
    | RegisterRole
        { platform :: Platform
        , repository :: Repository
        , role :: Role
        , username :: Username
        , tokenId :: TokenId
        }
    | UnregisterRole
        { platform :: Platform
        , repository :: Repository
        , role :: Role
        , username :: Username
        , tokenId :: TokenId
        }
    | RequestTest
        { platform :: Platform
        , repository :: Repository
        , commit :: SHA1
        , directory :: Directory
        , username :: Username
        , tokenId :: TokenId
        }
    | RetractRequest
        { outputReference :: OutputReference
        }
    | GetFacts
        { tokenId :: TokenId
        }
    deriving (Eq, Show)

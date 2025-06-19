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
    , Repository (..)
    , Request (..)
    , Role (..)
    , SHA1 (..)
    , TokenId (..)
    , Username (..)
    , Address (..)
    , WithUnsignedTx (..)
    , WithTxHash (..)
    , Wallet (..)
    , RequestRefId (..)
    , SignedTx (..)
    , UnsignedTx (..)
    , TxHash (..)
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
    , (.:?)
    )
import Data.Text (Text)
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Text.Read (readMaybe)

import qualified Data.Text as T

newtype RequestRefId = RequestRefId
    { requestId :: Text
    }
    deriving (Eq, Show)

instance ToHttpApiData RequestRefId where
    toUrlPiece (RequestRefId rid) = rid

instance FromHttpApiData RequestRefId where
    parseUrlPiece rid =
        case rid of
            "" -> Left "RequestRefId cannot be empty"
            _ -> Right (RequestRefId rid)
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

instance FromJSON TxId where
    parseJSON = withText "TxId" $ \v ->
        case readMaybe (T.unpack v) of
            Just txId -> pure (TxId txId)
            Nothing -> fail $ "Invalid TxId: " ++ T.unpack v

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

instance ToHttpApiData Operation where
    toUrlPiece Insert = "insert"
    toUrlPiece Delete = "delete"

instance FromHttpApiData Operation where
    parseUrlPiece v =
        case v of
            "insert" -> Right Insert
            "delete" -> Right Delete
            _ -> Left $ "Invalid operation: " <> v

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

newtype Options = Options
    { command :: Command
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
    = GetToken
    | UpdateToken
        { requests :: [RequestRefId]
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
        }
    | UnregisterPublicKey
        { platform :: Platform
        , username :: Username
        , pubkeyhash :: PublicKeyHash
        }
    | RegisterRole
        { platform :: Platform
        , repository :: Repository
        , role :: Role
        , username :: Username
        }
    | UnregisterRole
        { platform :: Platform
        , repository :: Repository
        , role :: Role
        , username :: Username
        }
    | RequestTest
        { platform :: Platform
        , repository :: Repository
        , commit :: SHA1
        , directory :: Directory
        , username :: Username
        }
    | RetractRequest
        { outputReference :: RequestRefId
        }
    | GetFacts
        {
        }
    deriving (Eq, Show)

newtype Address = Address Text

instance FromHttpApiData Address where
    parseUrlPiece addr =
        if T.null addr
            then Left "Address cannot be empty"
            else Right (Address addr)

instance ToHttpApiData Address where
    toUrlPiece (Address addr) = addr

newtype SignedTx = SignedTx
    { signedTransaction :: Text
    }
    deriving (Show)

instance ToJSON SignedTx where
    toJSON (SignedTx signedTransaction) =
        object
            [ "signedTransaction" .= signedTransaction
            ]

instance FromJSON SignedTx where
    parseJSON = withObject "SignedTx" $ \v ->
        SignedTx
            <$> v .: "signedTransaction"

newtype UnsignedTx = UnsignedTx
    { unsignedTransaction :: Text
    }
    deriving (Show)

instance ToJSON UnsignedTx where
    toJSON (UnsignedTx unsignedTransaction) =
        object
            [ "unsignedTransaction" .= unsignedTransaction
            ]

instance FromJSON UnsignedTx where
    parseJSON = withObject "UnsignedTx" $ \v ->
        UnsignedTx
            <$> v .: "unsignedTransaction"

data WithUnsignedTx = WithUnsignedTx
    { unsignedTransaction :: Text
    , value :: Maybe Value
    }
    deriving (Show)

instance ToJSON WithUnsignedTx where
    toJSON (WithUnsignedTx unsignedTransaction value) =
        object
            [ "unsignedTransaction" .= unsignedTransaction
            , "value" .= value
            ]

instance FromJSON WithUnsignedTx where
    parseJSON = withObject "WithUnsignedTx" $ \v ->
        WithUnsignedTx
            <$> v .: "unsignedTransaction"
            <*> v .:? "value"

newtype TxHash = TxHash
    { txHash :: Text
    }
    deriving (Eq, Show)

instance FromJSON TxHash where
    parseJSON = withObject "TxHash" $ \v ->
        TxHash
            <$> v .: "txHash"

instance ToJSON TxHash where
    toJSON (TxHash txHash) =
        object
            [ "txHash" .= txHash
            ]
data WithTxHash = WithTxHash
    { txHash :: Text
    , value :: Maybe Value
    }
    deriving (Show)

instance ToJSON WithTxHash where
    toJSON (WithTxHash txHash value) =
        object
            [ "txHash" .= txHash
            , "value" .= value
            ]

data Wallet = Wallet
    { address :: Address
    , sign :: Text -> Text
    }

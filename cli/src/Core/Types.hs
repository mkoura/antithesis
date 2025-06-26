{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module Core.Types
    ( Directory (..)
    , Host (..)
    , Operation (..)
    , Platform (..)
    , Port (..)
    , PublicKeyHash (..)
    , Repository (..)
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
    , Owner (..)
    , Key (..)
    , CageDatum (..)
    ) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Lib.JSON (object, withObject, (.:), (.=))
import PlutusTx (Data (..), builtinDataToData)
import PlutusTx.IsData.Class
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue
    , ReportSchemaErrors
    , ToJSON (..)
    )

-- TxHash-OutputIndex
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

instance Aeson.ToJSON RequestRefId where
    toJSON (RequestRefId ref) = Aeson.String ref

instance Aeson.FromJSON RequestRefId where
    parseJSON = Aeson.withText "RequestRefId" $ \txt ->
        pure $ RequestRefId txt

newtype TokenId = TokenId String
    deriving (Eq, Show)

instance FromData TokenId where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 0 [B b] -> Just (TokenId $ B.unpack $ encode b)
            _ -> Nothing

instance ToHttpApiData TokenId where
    toUrlPiece (TokenId tokenId) = T.pack tokenId

instance FromHttpApiData TokenId where
    parseUrlPiece tokenId =
        case T.unpack tokenId of
            "" -> Left "TokenId cannot be empty"
            _ -> Right (TokenId (T.unpack tokenId))

newtype Owner = Owner String
    deriving (Eq, Show)

instance FromData Owner where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            B b -> Just (Owner $ B.unpack $ encode b)
            _ -> Nothing

newtype Key = Key String
    deriving (Eq, Show)

instance FromData Key where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            B b -> Just (Key $ B.unpack b)
            _ -> Nothing

data Operation
    = Insert ByteString
    | Delete ByteString
    | Update ByteString ByteString
    deriving (Eq, Show)

instance FromData Operation where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 0 [B b] -> Just (Insert b)
            Constr 1 [B b] -> Just (Delete b)
            Constr 2 [B old, B new] -> Just (Update old new)
            _ -> Nothing

newtype Root = Root String
    deriving (Eq, Show)

instance FromData Root where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            B b -> Just (Root $ B.unpack $ encode b)
            _ -> Nothing

data CageDatum
    = RequestDatum
        { tokenId :: TokenId
        , owner :: Owner
        , key :: Key
        , value :: Operation
        }
    | StateDatum
        { owner :: Owner
        , root :: Root
        }
    deriving (Eq, Show)

instance FromData CageDatum where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 0 [tokenIdD, ownerD, keyD, valueD] ->
                RequestDatum
                    <$> fromData tokenIdD
                    <*> fromData ownerD
                    <*> fromData keyD
                    <*> fromData valueD
            Constr 1 [ownerD, rootD] ->
                StateDatum
                    <$> fromData ownerD
                    <*> fromData rootD
            _ -> Nothing

newtype Platform = Platform String
    deriving (Eq, Show)

newtype PublicKeyHash = PublicKeyHash String
    deriving (Eq, Show)

newtype SHA1 = SHA1 String
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

newtype Port = Port Int
    deriving (Eq, Show)

newtype Host = Host String
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

instance Aeson.FromJSON SignedTx where
    parseJSON = Aeson.withObject "SignedTx" $ \v ->
        SignedTx
            <$> v Aeson..: "signedTransaction"

instance Aeson.ToJSON SignedTx where
    toJSON (SignedTx signedTransaction) =
        Aeson.object
            [ "signedTransaction" Aeson..= signedTransaction
            ]
newtype UnsignedTx = UnsignedTx
    { unsignedTransaction :: Text
    }
    deriving (Show)

instance Monad m => ToJSON m UnsignedTx where
    toJSON (UnsignedTx unsignedTransaction) =
        object
            [ "unsignedTransaction" .= unsignedTransaction
            ]

instance ReportSchemaErrors m => FromJSON m UnsignedTx where
    fromJSON = withObject "UnsignedTx" $ \v ->
        UnsignedTx
            <$> v .: "unsignedTransaction"

data WithUnsignedTx a = WithUnsignedTx
    { unsignedTransaction :: Text
    , value :: Maybe a
    }
    deriving (Show, Functor, Eq)

instance (ToJSON m a, Monad m) => ToJSON m (WithUnsignedTx a) where
    toJSON (WithUnsignedTx unsignedTransaction value) =
        object
            [ "unsignedTransaction" .= unsignedTransaction
            , "value" .= value
            ]

instance (Aeson.FromJSON a) => Aeson.FromJSON (WithUnsignedTx a) where
    parseJSON = Aeson.withObject "WithUnsignedTx" $ \v ->
        WithUnsignedTx
            <$> v Aeson..: "unsignedTransaction"
            <*> v Aeson..:? "value"

instance (Aeson.ToJSON a) => Aeson.ToJSON (WithUnsignedTx a) where
    toJSON (WithUnsignedTx unsignedTransaction value) =
        Aeson.object
            [ "unsignedTransaction" Aeson..= unsignedTransaction
            , "value" Aeson..= value
            ]

newtype TxHash = TxHash
    { txHash :: Text
    }
    deriving (Eq, Show)

instance Aeson.FromJSON TxHash where
    parseJSON = Aeson.withObject "TxHash" $ \v ->
        TxHash
            <$> v Aeson..: "txHash"

instance Aeson.ToJSON TxHash where
    toJSON (TxHash txHash) =
        Aeson.object
            [ "txHash" Aeson..= txHash
            ]

instance Monad m => ToJSON m TxHash where
    toJSON (TxHash txHash) =
        object
            [ "txHash" .= txHash
            ]
data WithTxHash = WithTxHash
    { txHash :: Text
    , value :: Maybe JSValue
    }
    deriving (Show)

instance Monad m => ToJSON m WithTxHash where
    toJSON (WithTxHash txHash value) =
        object
            [ "txHash" .= txHash
            , "value" .= value
            ]

data Wallet = Wallet
    { address :: Address
    , sign :: UnsignedTx -> SignedTx
    }

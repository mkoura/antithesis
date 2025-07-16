{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Types
    ( Address (..)
    , CageDatum (..)
    , Change (..)
    , Directory (..)
    , Duration (..)
    , Fact (..)
    , Host (..)
    , Key (..)
    , Operation (..)
    , Owner (..)
    , Platform (..)
    , Port (..)
    , PublicKeyHash (..)
    , Repository (..)
    , RequestRefId (..)
    , Role (..)
    , Root (..)
    , Commit (..)
    , SignedTx (..)
    , SignTxError (..)
    , TokenId (..)
    , Try (..)
    , TxHash (..)
    , UnsignedTx (..)
    , Username (..)
    , Wallet (..)
    , WithTxHash (..)
    , WithUnsignedTx (..)
    , parseFacts
    , JSFact
    , Op (..)
    ) where

import Control.Exception (Exception)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lib.JSON
    ( object
    , parseJSValue
    , withObject
    , (.:)
    , (.=)
    )
import PlutusTx (Data (..), builtinDataToData)
import PlutusTx.IsData.Class (FromData (..), fromData)
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ReportSchemaErrors (..)
    , ToJSON (..)
    , expectedButGotValue
    , renderCanonicalJSON
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

instance Monad m => ToJSON m RequestRefId where
    toJSON (RequestRefId ref) = toJSON ref

instance ReportSchemaErrors m => FromJSON m RequestRefId where
    fromJSON v = RequestRefId <$> fromJSON v

instance Aeson.ToJSON RequestRefId where
    toJSON (RequestRefId ref) = Aeson.String ref

instance Aeson.FromJSON RequestRefId where
    parseJSON = Aeson.withText "RequestRefId" $ \txt ->
        pure $ RequestRefId txt

newtype TokenId = TokenId String
    deriving (Eq, Show)

instance Monad m => ToJSON m TokenId where
    toJSON (TokenId tokenId) = toJSON tokenId

instance ReportSchemaErrors m => FromJSON m TokenId where
    fromJSON v = TokenId <$> fromJSON v

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

instance Monad m => ToJSON m Owner where
    toJSON (Owner owner) = toJSON owner

instance ReportSchemaErrors m => FromJSON m Owner where
    fromJSON v = Owner <$> fromJSON v

instance FromData Owner where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            B b -> Just (Owner $ B.unpack $ encode b)
            _ -> Nothing

newtype Key a = Key a
    deriving (Eq, Show)

instance FromJSON Maybe a => FromData (Key a) where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            B b -> Key <$> parseJSValue b
            _ -> Nothing

instance (ToJSON m a, Monad m) => ToJSON m (Key a) where
    toJSON (Key key) = do
        j <- BL.unpack . renderCanonicalJSON <$> toJSON key
        toJSON j

instance
    (FromJSON m a, Monad m, ReportSchemaErrors m)
    => FromJSON m (Key a)
    where
    fromJSON v = do
        keyString :: String <- fromJSON v
        Key <$> parseJSValue (B.pack keyString)

data Op a b = OpI a | OpD b | OpU a b
    deriving (Show, Eq)

data Operation s where
    Insert :: a -> Operation (OpI a)
    Delete :: a -> Operation (OpD a)
    Update :: a -> b -> Operation (OpU a b)

deriving instance Show a => Show (Operation (OpI a))
deriving instance Show a => Show (Operation (OpD a))
deriving instance (Show a, Show b) => Show (Operation (OpU a b))

deriving instance Eq a => Eq (Operation (OpI a))
deriving instance Eq a => Eq (Operation (OpD a))
deriving instance (Eq a, Eq b) => Eq (Operation (OpU a b))

instance FromJSON Maybe a => FromData (Operation (OpI a)) where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 0 [B b] -> Insert <$> parseJSValue b
            _ -> Nothing
instance FromJSON Maybe a => FromData (Operation (OpD a)) where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 1 [B b] -> Delete <$> parseJSValue b
            _ -> Nothing

instance (FromJSON Maybe a, FromJSON Maybe b) => FromData (Operation (OpU a b)) where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 2 [B old, B new] -> do
                oldValue <- parseJSValue old
                newValue <- parseJSValue new
                pure $ Update oldValue newValue
            _ -> Nothing

instance (ToJSON m a, Monad m) => ToJSON m (Operation (OpI a)) where
    toJSON (Insert a) = do
        v <- BL.unpack . renderCanonicalJSON <$> toJSON a
        object
            ["type" .= ("insert" :: String), "value" .= v]

instance (ToJSON m a, Monad m) => ToJSON m (Operation (OpD a)) where
    toJSON (Delete a) = do
        v <- BL.unpack . renderCanonicalJSON <$> toJSON a
        object
            ["type" .= ("delete" :: String), "value" .= v]

instance (ToJSON m a, ToJSON m b, Monad m) => ToJSON m (Operation (OpU a b)) where
    toJSON (Update old new) = do
        oldValue <- BL.unpack . renderCanonicalJSON <$> toJSON old
        newValue <- BL.unpack . renderCanonicalJSON <$> toJSON new
        object
            [ "type" .= ("update" :: String)
            , "oldValue" .= oldValue
            , "newValue" .= newValue
            ]

instance
    (ReportSchemaErrors m, FromJSON m a)
    => FromJSON m (Operation (OpI a))
    where
    fromJSON = withObject "Operation" $ \v -> do
        op <- v .: "type"
        case op of
            JSString "insert" -> do
                valueString <- v .: "value"
                Insert <$> parseJSValue (B.pack valueString)
            _ -> expectedButGotValue "insert operation" op

instance
    (ReportSchemaErrors m, FromJSON m a)
    => FromJSON m (Operation (OpD a))
    where
    fromJSON = withObject "Operation" $ \v -> do
        op <- v .: "type"
        case op of
            JSString "delete" -> do
                valueString <- v .: "value"
                Delete <$> parseJSValue (B.pack valueString)
            _ -> expectedButGotValue "delete operation" op

instance
    (ReportSchemaErrors m, FromJSON m a, FromJSON m b)
    => FromJSON m (Operation (OpU a b))
    where
    fromJSON = withObject "Operation" $ \v -> do
        op <- v .: "type"
        case op of
            JSString "update" -> do
                oldValueString <- v .: "oldValue"
                oldValue <- parseJSValue (B.pack oldValueString)
                newValueString <- v .: "newValue"
                newValue <- parseJSValue (B.pack newValueString)
                pure $ Update oldValue newValue
            _ -> expectedButGotValue "Operation" op

newtype Root = Root String
    deriving (Eq, Show)

instance FromData Root where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            B b -> Just (Root $ B.unpack $ encode b)
            _ -> Nothing

instance Monad m => ToJSON m Root where
    toJSON (Root root) = toJSON root

instance (ReportSchemaErrors m) => FromJSON m Root where
    fromJSON v = Root <$> fromJSON v

data Change k op = Change
    { key :: Key k
    , operation :: Operation op
    }

deriving instance (Show k, Show (Operation op)) => Show (Change k op)
deriving instance (Eq k, Eq (Operation op)) => Eq (Change k op)

instance
    (FromJSON m k, FromJSON m (Operation op), ReportSchemaErrors m)
    => FromJSON m (Change k op)
    where
    fromJSON w = flip (withObject "Change") w $ \v -> do
        key <- v .: "key"
        operation <- fromJSON w
        pure $ Change key operation

instance
    (Monad m, ToJSON m k, ToJSON m (Operation op))
    => ToJSON m (Change k op)
    where
    toJSON (Change key operation) =
        object
            [ "key" .= key
            , "operation" .= operation
            ]

data CageDatum k op
    = RequestDatum
        { tokenId :: TokenId
        , owner :: Owner
        , change :: Change k op
        }
    | StateDatum
        { owner :: Owner
        , root :: Root
        }

deriving instance Show (Change k op) => Show (CageDatum k op)
deriving instance Eq (Change k op) => Eq (CageDatum k op)

instance
    (FromJSON Maybe k, FromData (Operation op))
    => FromData (CageDatum k op)
    where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 0 [tokenIdD, ownerD, keyD, valueD] ->
                RequestDatum
                    <$> fromData tokenIdD
                    <*> fromData ownerD
                    <*> (Change <$> fromData keyD <*> fromData valueD)
            Constr 1 [ownerD, rootD] ->
                StateDatum
                    <$> fromData ownerD
                    <*> fromData rootD
            _ -> Nothing

newtype Platform = Platform String
    deriving (Eq, Show)

newtype PublicKeyHash = PublicKeyHash String
    deriving (Eq, Show)

newtype Commit = Commit String
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
    deriving (Eq, Show)

instance Monad m => ToJSON m Address where
    toJSON (Address addr) = toJSON addr

instance ReportSchemaErrors m => FromJSON m Address where
    fromJSON v = Address <$> fromJSON v

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
    deriving (Show, Eq)

instance Aeson.FromJSON UnsignedTx where
    parseJSON = Aeson.withText "UnsignedTx" $ \v ->
        pure (UnsignedTx v)

instance Aeson.ToJSON UnsignedTx where
    toJSON (UnsignedTx unsignedTransaction) =
        Aeson.toJSON unsignedTransaction

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
    { unsignedTransaction :: UnsignedTx
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
    { textOf :: Text
    }
    deriving (Eq, Show)

instance FromHttpApiData TxHash where
    parseUrlPiece textOf =
        if T.null textOf
            then Left "TxHash cannot be empty"
            else Right (TxHash{textOf})

instance ToHttpApiData TxHash where
    toUrlPiece (TxHash txHash) = txHash
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
data WithTxHash a = WithTxHash
    { txHash :: TxHash
    , value :: Maybe a
    }
    deriving (Show)

instance (Monad m, ToJSON m a) => ToJSON m (WithTxHash a) where
    toJSON (WithTxHash (TxHash txHash) value) =
        object
            [ "txHash" .= txHash
            , "value" .= value
            ]

data SignTxError
    = InvalidTx
    | InvalidHex
    deriving (Show, Eq)

instance Exception SignTxError

data Wallet = Wallet
    { address :: Address
    , owner :: Owner
    , sign :: UnsignedTx -> Either SignTxError SignedTx
    }

newtype Duration = Duration Int
    deriving (Eq, Show)

newtype Try = Try Int
    deriving (Eq, Show, Ord, Enum)

data Fact k v = Fact
    { factKey :: k
    , factValue :: v
    }
    deriving (Eq, Show)

instance
    ( ReportSchemaErrors m
    , FromJSON m k
    , FromJSON m v
    )
    => FromJSON m (Fact k v)
    where
    fromJSON = withObject "Fact" $ \v -> do
        key <- v .: "key"
        value <- v .: "value"
        pure $ Fact key value

instance (Monad m, ToJSON m k, ToJSON m v) => ToJSON m (Fact k v) where
    toJSON (Fact key value) =
        object
            [ "key" .= key
            , "value" .= value
            ]

type JSFact = Fact JSValue JSValue

parseFacts
    :: (FromJSON Maybe key, FromJSON Maybe val) => JSValue -> [Fact key val]
parseFacts v = fromMaybe [] $ do
    factsJSON <- fromJSON v
    pure $ mapMaybe f factsJSON
  where
    f (Fact key value) = do
        key' <- fromJSON key
        value' <- fromJSON value
        Just $ Fact key' value'

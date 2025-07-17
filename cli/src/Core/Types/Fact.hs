{-# LANGUAGE StrictData #-}

module Core.Types.Fact
    ( Fact (..)
    , keyHash
    , parseFacts
    , JSFact
    ) where

import Crypto.Hash (Blake2b_256, Digest)
import Crypto.Hash qualified as Hash
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe, mapMaybe)
import Lib.JSON
    ( object
    , withObject
    , (.:)
    , (.=)
    )
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ReportSchemaErrors (..)
    , ToJSON (..)
    , renderCanonicalJSON
    )

data Fact k v = Fact
    { factKey :: k
    , factValue :: v
    }
    deriving (Eq, Show)

keyHash :: (ToJSON m k, Monad m) => k -> m String
keyHash key = do
    keyJ <- toJSON key
    pure
        $ B.unpack
        $ Base16.encode
        $ BA.convert @(Digest Blake2b_256)
        $ Hash.hash
        $ BL.toStrict
        $ renderCanonicalJSON keyJ
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
    toJSON (Fact key value) = do
        keyJ <- toJSON key
        idJ <- keyHash key
        object
            [ "key" .= keyJ
            , "value" .= value
            , "id" .= idJ
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

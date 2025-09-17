{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Core.Types.Fact
    ( Fact (..)
    , keyHash
    , parseFacts
    , JSFact
    , toJSFact
    , fromJSFact
    ) where

import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as B
import Data.Maybe (fromMaybe, mapMaybe)
import Lib.JSON.Canonical.Extra
    ( blakeHashOfJSON
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ReportSchemaErrors (..)
    , ToJSON (..)
    )

data Fact k v = Fact
    { factKey :: k
    , factValue :: v
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

keyHash :: (ToJSON m k, Monad m) => k -> m String
keyHash key = do
    B.unpack
        . Base16.encode
        <$> blakeHashOfJSON key
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

toJSFact :: (ToJSON m k, ToJSON m v, Monad m) => k -> v -> m JSFact
toJSFact key value = do
    keyJ <- toJSON key
    valueJ <- toJSON value
    return $ Fact keyJ valueJ

fromJSFact
    :: (FromJSON m k, FromJSON m v, Monad m) => JSFact -> m (Fact k v)
fromJSFact (Fact key value) = do
    key' <- fromJSON key
    value' <- fromJSON value
    return $ Fact key' value'

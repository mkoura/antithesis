{-# LANGUAGE StrictData #-}

module Core.Types.Fact
    ( Fact (..)
    , parseFacts
    , JSFact
    ) where

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
    )

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

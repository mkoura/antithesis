{-# LANGUAGE DeriveFunctor #-}

module Lib.JSON
    ( getField
    , getStringField
    , getIntegralField
    , getStringMapField
    , object
    , intJSON
    , stringJSON
    , getListField
    , jsonToString
    , toAeson
    , CanonicalJSON (..)
    , CanonicalJSONError (..)
    , runIdentityCanonicalJSON
    )
where

import Control.Monad ((<=<))
import Data.Aeson (Value)
import Data.Aeson.Decoding (decode)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Text.JSON.Canonical
    ( FromJSON (fromJSON)
    , Int54
    , ReportSchemaErrors (..)
    , ToJSON (..)
    , renderCanonicalJSON
    )
import Text.JSON.Canonical.Types (JSValue)

getField
    :: ReportSchemaErrors m => String -> Map String JSValue -> m JSValue
getField key mapping = case Map.lookup key mapping of
    Nothing -> expected (key <> " key") Nothing
    Just value -> pure value

getStringField
    :: ReportSchemaErrors m
    => String
    -> Map String JSValue
    -> m String
getStringField key mapping = getField key mapping >>= fromJSON

getListField
    :: ReportSchemaErrors m
    => String
    -> Map String JSValue
    -> m [JSValue]
getListField key mapping = getField key mapping >>= fromJSON

getIntegralField
    :: (ReportSchemaErrors m, Num a)
    => String
    -> Map String JSValue
    -> m a
getIntegralField key mapping =
    getField key mapping >>= fromJSON @_ @Int54 <&> fromIntegral

getStringMapField
    :: ReportSchemaErrors m
    => String
    -> Map String JSValue
    -> m (Map String JSValue)
getStringMapField key mapping = getField key mapping >>= fromJSON

object :: (Monad m, ToJSON m a) => [(String, m a)] -> m JSValue
object xs = toJSON <=< sequence $ Map.fromList xs

intJSON :: (Monad m, Integral a) => a -> m JSValue
intJSON = toJSON @_ @Int54 . fromIntegral

stringJSON :: Monad m => String -> m JSValue
stringJSON = toJSON

jsonToString :: JSValue -> String
jsonToString = BL.unpack . renderCanonicalJSON

-- TODO: do the conversion in a more efficient way
toAeson :: JSValue -> Maybe Value
toAeson = decode . renderCanonicalJSON

data CanonicalJSONError = CanonicalJSONError
    { expectedValue :: String
    , gotValue :: Maybe String
    }
    deriving (Show, Eq)

-- >>> import Data.Functor.Identity
-- >>> pure 1 :: CanonicalJSON Identity Int
newtype CanonicalJSON m a = CanonicalJSON
    {runCanonicalJSON :: m (Either CanonicalJSONError a)}
    deriving (Functor)

instance Applicative m => Applicative (CanonicalJSON m) where
    pure x = CanonicalJSON $ pure $ Right x
    CanonicalJSON f <*> CanonicalJSON x = CanonicalJSON $ liftA2 (<*>) f x

instance Monad m => Monad (CanonicalJSON m) where
    CanonicalJSON x >>= f = CanonicalJSON $ do
        result <- x
        case result of
            Left err -> pure $ Left err
            Right value -> runCanonicalJSON $ f value

instance Monad m => ReportSchemaErrors (CanonicalJSON m) where
    expected expectedValue gotValue =
        CanonicalJSON
            $ pure
            $ Left
            $ CanonicalJSONError{expectedValue, gotValue}

runIdentityCanonicalJSON
    :: CanonicalJSON Identity a -> Either CanonicalJSONError a
runIdentityCanonicalJSON (CanonicalJSON x) = runIdentity x

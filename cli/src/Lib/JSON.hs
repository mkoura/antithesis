module Lib.JSON
    ( getField
    , getStringField
    , getIntegralField
    , getStringMapField
    , object
    , intJSON
    , stringJSON
    , getListField
    )
where

import Control.Monad ((<=<))
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Text.JSON.Canonical
    ( FromJSON (fromJSON)
    , Int54
    , ReportSchemaErrors (..)
    , ToJSON (..)
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

module Lib.JSON
    ( getField
    , getStringField
    , getIntegralField
    , getStringMapField
    , ReportSchemaErrors (..)
    )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Text.JSON.Canonical
    ( FromJSON (fromJSON)
    , JSValue (JSNum, JSObject, JSString)
    , ReportSchemaErrors (..)
    , fromJSString
    )

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
getStringField key mapping = do
    value <- getField key mapping
    case value of
        JSString jsString -> pure $ fromJSString jsString
        _ -> expected ("a stringy " <> key) $ Just "got something else"

getIntegralField
    :: (ReportSchemaErrors m, Num a)
    => String
    -> Map String JSValue
    -> m a
getIntegralField key mapping = do
    value <- getField key mapping
    case value of
        JSNum n -> pure $ fromIntegral n
        _ -> expected ("an integer " <> key) $ Just "got something else"

getStringMapField
    :: ReportSchemaErrors m
    => String
    -> Map String JSValue
    -> m (Map String JSValue)
getStringMapField key mapping = do
    value <- getField key mapping
    case value of
        JSObject _ -> do
            objMapping :: Map String JSValue <- fromJSON value
            pure objMapping
        _ -> expected ("a map " <> key) $ Just "got something else"

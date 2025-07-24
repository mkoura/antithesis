module Lib.CanonAeson (fromAeson, fromCanon, fromScientific) where

import qualified Data.Aeson             as A
import qualified Data.Aeson.Key         as K
import qualified Data.Aeson.KeyMap      as KM
import           Data.Maybe             (fromJust)
import           Data.Scientific        ( Scientific
                                        , toBoundedInteger
                                        , scientific
                                        )
import           Data.Text              (pack, unpack)
import qualified Data.Vector            as V

import qualified Text.JSON.Canonical as C

fromCanon :: C.JSValue -> A.Value
fromCanon (C.JSBool b)    = A.Bool b
fromCanon C.JSNull        = A.Null
fromCanon (C.JSNum n)     = A.Number (scientific (fromIntegral n) 0)
fromCanon (C.JSString s)  = A.String $ pack $ C.fromJSString s
fromCanon (C.JSArray xs)  = A.Array $ V.fromList $ fmap fromCanon xs
fromCanon (C.JSObject xs) = A.Object $ KM.fromList $ map fromCanonKV xs

fromAeson :: A.Value -> Maybe C.JSValue
fromAeson (A.Bool b)    = Just (C.JSBool b)
fromAeson A.Null        = Just C.JSNull
fromAeson (A.Number x)  = fromScientific x
fromAeson (A.Array xs)  = C.JSArray <$> traverse fromAeson (V.toList xs)
fromAeson (A.String t)  = Just $ C.JSString $ C.toJSString $ unpack t
fromAeson (A.Object xs) = Just $ C.JSObject $ map fromAesonKV $ KM.toList xs

fromScientific :: Scientific -> Maybe C.JSValue
fromScientific = fmap C.JSNum . toBoundedInteger

fromCanonKV :: (C.JSString, C.JSValue) -> (K.Key, A.Value)
fromCanonKV (k,v) = (K.fromText $ pack $ C.fromJSString k, fromCanon v)

fromAesonKV :: (K.Key, A.Value) -> (C.JSString, C.JSValue)
fromAesonKV (k,v) = (C.toJSString $ unpack $ K.toText k, fromJust $ fromAeson v)

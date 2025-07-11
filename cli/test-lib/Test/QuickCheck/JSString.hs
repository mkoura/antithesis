module Test.QuickCheck.JSString
    ( JSStringValueChar (..)
    , JSStringValue (..)
    )
where

import Test.QuickCheck (Arbitrary (..), elements, listOf)

chars :: String
chars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> [' ', '-', '_']

newtype JSStringValueChar = JSStringValueChar {getJSStringValueChar :: Char}
    deriving (Show, Eq)

instance Arbitrary JSStringValueChar where
    arbitrary = JSStringValueChar <$> elements chars

newtype JSStringValue = JSStringValue {getJSStringValue :: String}
    deriving (Show, Eq)

instance Arbitrary JSStringValue where
    arbitrary = JSStringValue . fmap getJSStringValueChar <$> listOf arbitrary

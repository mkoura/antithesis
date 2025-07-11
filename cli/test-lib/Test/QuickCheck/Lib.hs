module Test.QuickCheck.Lib
    ( withAPresence
    ) where

import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , frequency
    , suchThat
    )

withAPresence :: (Arbitrary a, Eq a) => Float -> a -> Gen a
withAPresence presence a = do
    b <- arbitrary `suchThat` (/= a)
    let presence100 = round (presence * 100)
    frequency
        [ (presence100, return a)
        , (100 - presence100, return b)
        ]

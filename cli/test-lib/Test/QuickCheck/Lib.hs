module Test.QuickCheck.Lib
    ( withAPresence
    , withNothing) where

import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , frequency
    , suchThat
    )

oneOrTheOther :: Float -> Gen a -> Gen a -> Gen a
oneOrTheOther presence genA genB = do
    let presence100 = round (presence * 100)
    frequency
        [ (presence100, genA)
        , (100 - presence100, genB)
        ]

withAPresence :: (Arbitrary a, Eq a) => Float -> a -> Gen a
withAPresence presence a = do
    b <- arbitrary `suchThat` (/= a)
    oneOrTheOther presence
        (return a)
        (return b)

withNothing :: Float -> Gen a -> Gen (Maybe a)
withNothing presence gen = do
    bool <- oneOrTheOther presence (return True) (return False)
    if bool
        then Just <$> gen
        else return Nothing
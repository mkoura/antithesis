{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MPFS.TypesSpec
    ( spec
    )
where

import Core.Types
    ( Key (..)
    , Owner (..)
    , RequestRefId (..)
    , Root (..)
    , Val (..)
    )
import MPFS.Types
    ( MPFSGetToken (..)
    , MPFSOperation (..)
    , MPFSRequest (..)
    , MPFSRequestChange (..)
    , MPFSTokenState (..)
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ReportSchemaErrors (..)
    , ToJSON (..)
    , toJSString
    )

instance ReportSchemaErrors IO where
    expected expct (Just got) =
        fail
            $ "Expected: "
                ++ expct
                ++ ", but got: "
                ++ got
    expected expct Nothing = fail $ "Expected: " ++ expct

roundTrip :: (ToJSON IO a, FromJSON IO a, Show a, Eq a) => a -> IO ()
roundTrip value = do
    encoded <- toJSON value
    decoded <- fromJSON encoded
    decoded `shouldBe` value

toCanonicalString :: String -> JSValue
toCanonicalString = JSString . toJSString

spec :: Spec
spec = do
    describe "MPFSRequestChange" $ do
        it "roundtrips on the JSON instance" $ do
            let testReqChange =
                    MPFSRequestChange
                        { key = Key $ toCanonicalString "key"
                        , value = Val $ toCanonicalString "val"
                        , operation = InsertOp
                        }
            roundTrip testReqChange

    describe "MPFSRequest" $ do
        it "roundtrips on the JSON instance" $ do
            let reqChange =
                    MPFSRequestChange
                        { key = Key $ toCanonicalString "key"
                        , value = Val $ toCanonicalString "val"
                        , operation = InsertOp
                        }
                testReq =
                    MPFSRequest
                        { requestOutput = RequestRefId "txhash-0"
                        , change = reqChange
                        , owner = Owner "owner"
                        }
            roundTrip testReq

    describe "MPFSTokenState" $ do
        it "roundtrips on the JSON instance" $ do
            let testTokenState =
                    MPFSTokenState
                        { owner = Owner "owner"
                        , root = Root "00000"
                        }
            roundTrip testTokenState

    describe "MPFSGetToken" $ do
        it "roundtrips on the JSON instance" $ do
            let change1 =
                    MPFSRequestChange
                        { key = Key $ toCanonicalString "key"
                        , value = Val $ toCanonicalString "val"
                        , operation = InsertOp
                        }
                req1 =
                    MPFSRequest
                        { requestOutput = RequestRefId "txhash-0"
                        , change = change1
                        , owner = Owner "owner"
                        }
                change2 =
                    MPFSRequestChange
                        { key = Key $ toCanonicalString "key"
                        , value = Val $ toCanonicalString "val1"
                        , operation = InsertOp
                        }
                req2 =
                    MPFSRequest
                        { requestOutput = RequestRefId "txhash-2"
                        , change = change2
                        , owner = Owner "owner2"
                        }
                tokenState =
                    MPFSTokenState
                        { owner = Owner "owner"
                        , root = Root "00000"
                        }
                testGetToken =
                    MPFSGetToken
                        { outputRefId = RequestRefId "txhash-0"
                        , state = tokenState
                        , requests = [req1, req2]
                        }
            roundTrip testGetToken

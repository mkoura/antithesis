{-# OPTIONS_GHC -Wno-orphans #-}

module User.TypesSpec
    ( spec
    )
where

import Core.Types
    ( Directory (Directory)
    , Platform (Platform)
    , Repository (Repository, organization, project)
    , SHA1 (SHA1)
    , Username (Username)
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.JSON.Canonical
    ( FromJSON (..)
    , ReportSchemaErrors (..)
    , ToJSON (..)
    )
import User.Types
    ( TestRun
        ( TestRun
        , commitId
        , directory
        , platform
        , repository
        , requester
        , testRunIndex
        )
    )

instance ReportSchemaErrors IO where
    expected expct (Just got) =
        fail
            $ "Expected: "
                ++ expct
                ++ ", but got: "
                ++ got
    expected expct Nothing = fail $ "Expected: " ++ expct

spec :: Spec
spec = do
    describe "TestRun" $ do
        it "roundtrips on the JSON instance" $ do
            let testRun =
                    TestRun
                        { platform = Platform "github"
                        , repository =
                            Repository
                                { organization = "user"
                                , project = "repo"
                                }
                        , commitId = SHA1 "abc123"
                        , directory = Directory "src"
                        , requester = Username "tester"
                        , testRunIndex = 1
                        }
            encoded <- toJSON testRun
            decoded <- fromJSON encoded
            decoded `shouldBe` testRun

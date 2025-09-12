{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module User.Agent.PublishResults.EmailSpec (spec)
where

import Core.Types.Basic
    ( Commit (Commit)
    , Directory (Directory)
    , Platform (Platform)
    , Repository (Repository, organization, project)
    , Username (Username)
    )
import Data.ByteString qualified as B
import Data.Maybe (fromJust)
import Data.String.QQ (s)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
    ( UTCTime (..)
    , defaultTimeLocale
    , diffUTCTime
    , parseTimeM
    , secondsToDiffTime
    )
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, choose, forAll, withMaxSuccess)
import User.Agent.PublishResults.Email
    ( ParsingError (..)
    , Result (..)
    , WithDateError (..)
    , clockTimeToUTCTime
    , readEmail
    , utcTimeToClockTime
    )
import User.Types
    ( TestRun
        ( TestRun
        , commitId
        , directory
        , platform
        , repository
        , requester
        , tryIndex
        )
    )

spec :: Spec
spec = do
    describe "old-time convertion" $ do
        prop "almost roundtrips" $ withMaxSuccess 100000 $ forAll genTime $ \t ->
            diffUTCTime (clockTimeToUTCTime (utcTimeToClockTime t)) t < 1e-9
    describe "email parser" $ do
        it "parses good email"
            $ case readEmail goldenEmail of
                Left _ -> expectationFailure "should parse"
                Right r ->
                    r
                        `shouldBe` Result
                            { description =
                                TestRun
                                    { commitId = Commit "a7741a44dfddfe05822e1a49862ceea43ecd657d"
                                    , directory = Directory "antithesis-test"
                                    , platform = Platform "github"
                                    , repository =
                                        Repository
                                            { organization =
                                                "cardano-foundation"
                                            , project = "hal-fixture-sin"
                                            }
                                    , requester = Username "cfhal"
                                    , tryIndex = 1
                                    }
                            , date =
                                fromJust
                                    $ parseTimeM
                                        True
                                        defaultTimeLocale
                                        "%d %m %Y %H:%M:%S"
                                        "05 09 2025 17:27:03"
                            , link = T.strip expectedLink
                            }
        it "rejects email with no date"
            $ case readEmail noDate of
                Left NoDate -> return ()
                Left _ -> expectationFailure "wrong error"
                Right _ -> expectationFailure "should not parse"
        it "rejects email with no description"
            $ case readEmail descriptionMissing of
                Left (WithDate _ DescriptionMissingOrUnusable) -> return ()
                Left _ -> expectationFailure "wrong error"
                Right _ -> expectationFailure "should not parse"
        it "rejects email with no link"
            $ case readEmail linkMissing of
                Left (WithDate _ LinkMissing) -> return ()
                Left _ -> expectationFailure "wrong error"
                Right _ -> expectationFailure "should not parse"

genTime :: Gen UTCTime
genTime = do
    day <- choose (0, 40000)
    sec <- choose (0, 86400)
    pico <- choose (0, floor @Double 1e12)
    return
        $ UTCTime
            { utctDay = toEnum day
            , utctDayTime = secondsToDiffTime sec + toEnum pico
            }

expectedLink :: Text
expectedLink =
    [s|
https://cardano.antithesis.com/report/zMNsYjs1lOgRg0IijZGTB1GN/mcuSRCDOzItEqm33oW5hyJHB-GLiVUEItcy_QHALwNg.html?auth=v2.public.eyJuYmYiOiIyMDI1LTA5LTA1VDE2OjI3OjAwLjE3OTczNTk5OFoiLCJzY29wZSI6eyJSZXBvcnRTY29wZVYxIjp7ImFzc2V0IjoibWN1U1JDRE96SXRFcW0zM29XNWh5SkhCLUdMaVZVRUl0Y3lfUUhBTHdOZy5odG1sIiwicmVwb3J0X2lkIjoiek1Oc1lqczFsT2dSZzBJaWpaR1RCMUdOIn19fXphuf6Ej7hBlNswCpx1nhpDqVndh8T9e-0_huYlJKnckuN9bTSlL8YrbjwBx6J5NrW50gs2EQClq15Ze2J7qQQ
|]

goldenEmail :: B.ByteString
goldenEmail =
    [s|
From: "'Antithesis Reports' via list_antithesis_external" <antithesis@cardanofoundation.org>
MIME-Version: 1.0
Content-Type: text/html; charset=UTF-8
Content-Transfer-Encoding: 7bit
Date: Fri, 5 Sep 2025 17:27:03 +0000

<br>Run by cardano on 2025-09-05 15:59 UTC<br> Description: {"commitId":"a7741a44dfddfe05822e1a49862ceea43ecd657d","directory":"antithesis-test","platform":"github","repository":{"organization":"cardano-foundation","repo":"hal-fixture-sin"},"requester":"cfhal","try":1,"type":"test-run"}<br><span style="font-size:larger;font-weight:bold"><a href=https://cardano.antithesis.com/report/zMNsYjs1lOgRg0IijZGTB1GN/mcuSRCDOzItEqm33oW5hyJHB-GLiVUEItcy_QHALwNg.html?auth=v2.public.eyJuYmYiOiIyMDI1LTA5LTA1VDE2OjI3OjAwLjE3OTczNTk5OFoiLCJzY29wZSI6eyJSZXBvcnRTY29wZVYxIjp7ImFzc2V0IjoibWN1U1JDRE96SXRFcW0zM29XNWh5SkhCLUdMaVZVRUl0Y3lfUUhBTHdOZy5odG1sIiwicmVwb3J0X2lkIjoiek1Oc1lqczFsT2dSZzBJaWpaR1RCMUdOIn19fXphuf6Ej7hBlNswCpx1nhpDqVndh8T9e-0_huYlJKnckuN9bTSlL8YrbjwBx6J5NrW50gs2EQClq15Ze2J7qQQ>View report - 156ddbac4c6eede1268ca1995cb18c4a-37-4</a></span><br><h3>No findings introduced this run.<br><font style="color:orange">2 ongoing</font> issues.</h3>



    <a href="https://cardano.antithesis.com/report/zMNsYjs1lOgRg0IijZGTB1GN/mcuSRCDOzItEqm33oW5hyJHB-GLiVUEItcy_QHALwNg.html?auth=v2.public.eyJuYmYiOiIyMDI1LTA5LTA1VDE2OjI3OjAwLjE3OTczNTk5OFoiLCJzY29wZSI6eyJSZXBvcnRTY29wZVYxIjp7ImFzc2V0IjoibWN1U1JDRE96SXRFcW0zM29XNWh5SkhCLUdMaVZVRUl0Y3lfUUhBTHdOZy5odG1sIiwicmVwb3J0X2lkIjoiek1Oc1lqczFsT2dSZzBJaWpaR1RCMUdOIn19fXphuf6Ej7hBlNswCpx1nhpDqVndh8T9e-0_huYlJKnckuN9bTSlL8YrbjwBx6J5NrW50gs2EQClq15Ze2J7qQQ#/run/156ddbac4c6eede1268ca1995cb18c4a-37-4/findings/2f95173b159955ee457c5f52cbb711791c742ef1,961d3c463f5ef7640df4b3f30c6d7d9d2759b9c7">Look into 2 ongoing findings.</a>

|]

noDate :: B.ByteString
noDate =
    [s|
From: "'Antithesis Reports' via list_antithesis_external" <antithesis@cardanofoundation.org>
MIME-Version: 1.0
Content-Type: text/html; charset=UTF-8
Content-Transfer-Encoding: 7bit

<br>Run by cardano on 2025-09-05 15:59 UTC<br> Description: {"commitId":"a7741a44dfddfe05822e1a49862ceea43ecd657d","directory":"antithesis-test","platform":"github","repository":{"organization":"cardano-foundation","repo":"hal-fixture-sin"},"requester":"cfhal","try":1,"type":"test-run"}<br><span style="font-size:larger;font-weight:bold"><a href=https://cardano.antithesis.com/report/zMNsYjs1lOgRg0IijZGTB1GN/mcuSRCDOzItEqm33oW5hyJHB-GLiVUEItcy_QHALwNg.html?auth=v2.public.eyJuYmYiOiIyMDI1LTA5LTA1VDE2OjI3OjAwLjE3OTczNTk5OFoiLCJzY29wZSI6eyJSZXBvcnRTY29wZVYxIjp7ImFzc2V0IjoibWN1U1JDRE96SXRFcW0zM29XNWh5SkhCLUdMaVZVRUl0Y3lfUUhBTHdOZy5odG1sIiwicmVwb3J0X2lkIjoiek1Oc1lqczFsT2dSZzBJaWpaR1RCMUdOIn19fXphuf6Ej7hBlNswCpx1nhpDqVndh8T9e-0_huYlJKnckuN9bTSlL8YrbjwBx6J5NrW50gs2EQClq15Ze2J7qQQ>View report - 156ddbac4c6eede1268ca1995cb18c4a-37-4</a></span><br><h3>No findings introduced this run.<br><font style="color:orange">2 ongoing</font> issues.</h3>



    <a href="https://cardano.antithesis.com/report/zMNsYjs1lOgRg0IijZGTB1GN/mcuSRCDOzItEqm33oW5hyJHB-GLiVUEItcy_QHALwNg.html?auth=v2.public.eyJuYmYiOiIyMDI1LTA5LTA1VDE2OjI3OjAwLjE3OTczNTk5OFoiLCJzY29wZSI6eyJSZXBvcnRTY29wZVYxIjp7ImFzc2V0IjoibWN1U1JDRE96SXRFcW0zM29XNWh5SkhCLUdMaVZVRUl0Y3lfUUhBTHdOZy5odG1sIiwicmVwb3J0X2lkIjoiek1Oc1lqczFsT2dSZzBJaWpaR1RCMUdOIn19fXphuf6Ej7hBlNswCpx1nhpDqVndh8T9e-0_huYlJKnckuN9bTSlL8YrbjwBx6J5NrW50gs2EQClq15Ze2J7qQQ#/run/156ddbac4c6eede1268ca1995cb18c4a-37-4/findings/2f95173b159955ee457c5f52cbb711791c742ef1,961d3c463f5ef7640df4b3f30c6d7d9d2759b9c7">Look into 2 ongoing findings.</a>

|]

descriptionMissing :: B.ByteString
descriptionMissing =
    [s|
From: "'Antithesis Reports' via list_antithesis_external" <antithesis@cardanofoundation.org>
MIME-Version: 1.0
Content-Type: text/html; charset=UTF-8
Content-Transfer-Encoding: 7bit
Date: Fri, 5 Sep 2025 17:27:03 +0000

<br>Run by cardano on 2025-09-05 15:59 UTC<br> Description: anything<br><span style="font-size:larger;font-weight:bold"><a href=https://cardano.antithesis.com/report/zMNsYjs1lOgRg0IijZGTB1GN/mcuSRCDOzItEqm33oW5hyJHB-GLiVUEItcy_QHALwNg.html?auth=v2.public.eyJuYmYiOiIyMDI1LTA5LTA1VDE2OjI3OjAwLjE3OTczNTk5OFoiLCJzY29wZSI6eyJSZXBvcnRTY29wZVYxIjp7ImFzc2V0IjoibWN1U1JDRE96SXRFcW0zM29XNWh5SkhCLUdMaVZVRUl0Y3lfUUhBTHdOZy5odG1sIiwicmVwb3J0X2lkIjoiek1Oc1lqczFsT2dSZzBJaWpaR1RCMUdOIn19fXphuf6Ej7hBlNswCpx1nhpDqVndh8T9e-0_huYlJKnckuN9bTSlL8YrbjwBx6J5NrW50gs2EQClq15Ze2J7qQQ>View report - 156ddbac4c6eede1268ca1995cb18c4a-37-4</a></span><br><h3>No findings introduced this run.<br><font style="color:orange">2 ongoing</font> issues.</h3>



    <a href="https://cardano.antithesis.com/report/zMNsYjs1lOgRg0IijZGTB1GN/mcuSRCDOzItEqm33oW5hyJHB-GLiVUEItcy_QHALwNg.html?auth=v2.public.eyJuYmYiOiIyMDI1LTA5LTA1VDE2OjI3OjAwLjE3OTczNTk5OFoiLCJzY29wZSI6eyJSZXBvcnRTY29wZVYxIjp7ImFzc2V0IjoibWN1U1JDRE96SXRFcW0zM29XNWh5SkhCLUdMaVZVRUl0Y3lfUUhBTHdOZy5odG1sIiwicmVwb3J0X2lkIjoiek1Oc1lqczFsT2dSZzBJaWpaR1RCMUdOIn19fXphuf6Ej7hBlNswCpx1nhpDqVndh8T9e-0_huYlJKnckuN9bTSlL8YrbjwBx6J5NrW50gs2EQClq15Ze2J7qQQ#/run/156ddbac4c6eede1268ca1995cb18c4a-37-4/findings/2f95173b159955ee457c5f52cbb711791c742ef1,961d3c463f5ef7640df4b3f30c6d7d9d2759b9c7">Look into 2 ongoing findings.</a>

|]

linkMissing :: B.ByteString
linkMissing =
    [s|
From: "'Antithesis Reports' via list_antithesis_external" <antithesis@cardanofoundation.org>
MIME-Version: 1.0
Content-Type: text/html; charset=UTF-8
Content-Transfer-Encoding: 7bit
Date: Fri, 5 Sep 2025 17:27:03 +0000

<br>Run by cardano on 2025-09-05 15:59 UTC<br> Description: {"commitId":"a7741a44dfddfe05822e1a49862ceea43ecd657d","directory":"antithesis-test","platform":"github","repository":{"organization":"cardano-foundation","repo":"hal-fixture-sin"},"requester":"cfhal","try":1,"type":"test-run"}<br><span style="font-size:larger;font-weight:bold"></span><br><h3>No findings introduced this run.<br><font style="color:orange">2 ongoing</font> issues.</h3>





|]

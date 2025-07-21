{-# LANGUAGE DuplicateRecordFields #-}

module Oracle.Validate.Requests.TestRun.AcceptSpec (spec)
where

import Core.Types.Basic
    ( Duration (..)
    )
import Core.Types.Fact (toJSFact)
import Oracle.Validate.Requests.TestRun.Lib
    ( mkValidation
    , noValidation
    , signatureGen
    , testRunEGen
    )
import Oracle.Validate.Requests.TestRun.Others
    ( AgentRejection (..)
    , validateToRunningCore
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldReturn
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Testable (..)
    , counterexample
    , oneof
    )
import Test.QuickCheck.EGen (egenProperty, gen, genA)
import Test.QuickCheck.Property (cover)
import User.Types
    ( TestRunState (..)
    )

spec :: Spec
spec = do
    describe "validate agent requests" $ do
        it "validate an accept test run" $ egenProperty $ do
            testRun <- testRunEGen
            signature <- gen signatureGen
            let pendingState = Pending (Duration 5) signature
            testRunFact <- toJSFact testRun pendingState
            let validation = mkValidation [testRunFact] [] [] []
                newTestRunState = Accepted pendingState
                test = validateToRunningCore validation testRun newTestRunState
            pure $ test `shouldReturn` Nothing

        it "fail to validate an accept for a non-existing test run"
            $ egenProperty
            $ do
                testRun <- testRunEGen
                signature <- gen signatureGen
                duration <- genA
                let pendingState = Pending (Duration duration) signature
                    newTestRunState = Accepted pendingState
                    test = validateToRunningCore noValidation testRun newTestRunState
                pure $ test `shouldReturn` Just PreviousStateWrong

        it
            "fail to validate an accept for a pending test run with different state"
            $ egenProperty
            $ do
                duration <- genA
                differentDuration <- gen $ oneof [arbitrary, pure duration]
                testRun <- testRunEGen
                signature <- gen signatureGen
                differentSignature <- gen $ oneof [signatureGen, pure signature]
                let fact = Pending (Duration duration) signature
                    request =
                        Pending (Duration differentDuration) differentSignature
                testRunFact <- toJSFact testRun fact
                let validation = mkValidation [testRunFact] [] [] []
                    newTestRunState = Accepted request
                    test = validateToRunningCore validation testRun newTestRunState
                pure
                    $ counterexample (show (fact, request))
                    $ cover 0.2 (fact == request) "enough success"
                    $ cover 0.7 (fact /= request) "enough failure"
                    $ property
                    $ do
                        if fact == request
                            then test `shouldReturn` Nothing
                            else test `shouldReturn` Just PreviousStateWrong

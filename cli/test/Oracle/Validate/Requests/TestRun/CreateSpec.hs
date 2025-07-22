{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Oracle.Validate.Requests.TestRun.CreateSpec (spec)
where

import Control.Lens ((%~), (.~))
import Core.Types.Basic
    ( Duration (..)
    , organization
    , project
    )
import Core.Types.Fact (JSFact, toJSFact)
import Lib.SSH.Public (encodePublicKey)
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Requests.TestRun.Create
    ( TestRunRejection (..)
    , validateCreateTestRunCore
    )
import Oracle.Validate.Requests.TestRun.Lib
    ( changeDirectory
    , changeOrganization
    , changePlatform
    , changeProject
    , changeRequester
    , changeTry
    , gitCommit
    , gitDirectory
    , jsFactRole
    , jsFactUser
    , mkValidation
    , noValidation
    , signTestRun
    , signatureGen
    , testConfigEGen
    , testRunEGen
    )
import Oracle.Validate.Types
    ( AValidationResult (ValidationFailure, ValidationSuccess)
    , Validated (..)
    , ValidationResult
    , runValidate
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldContain
    , shouldNotContain
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Positive (..)
    , Testable (..)
    , counterexample
    , cover
    , elements
    , listOf
    , oneof
    , suchThat
    )
import Test.QuickCheck.Crypton (sshGen)
import Test.QuickCheck.EGen
    ( egenProperty
    , gen
    , genA
    , genBlind
    , genShrinkA
    )
import User.Types
    ( TestRun (..)
    , TestRunRejection (..)
    , TestRunState (..)
    , URL (..)
    , tryIndexL
    )

shouldHaveReason
    :: (Show a, Eq a) => ValidationResult [a] -> a -> IO ()
shouldHaveReason (ValidationSuccess _) _ = pure ()
shouldHaveReason (ValidationFailure reasons) reason =
    reasons `shouldContain` [reason]

shouldNotHaveReason
    :: (Show a, Eq a) => ValidationResult [a] -> a -> IO ()
shouldNotHaveReason (ValidationSuccess _) _ = pure ()
shouldNotHaveReason (ValidationFailure reasons) reason =
    reasons `shouldNotContain` [reason]

onConditionHaveReason
    :: (Show a, Eq a) => ValidationResult [a] -> a -> Bool -> IO ()
onConditionHaveReason result reason = \case
    True -> shouldHaveReason result reason
    False -> shouldNotHaveReason result reason

spec :: Spec
spec = do
    describe "validateRequest" $ do
        it "accepts valid test run" $ egenProperty $ do
            testRun <- testRunEGen
            testConfig <- testConfigEGen
            Positive duration <-
                gen
                    $ arbitrary
                        `suchThat` \(Positive d) ->
                            d >= testConfig.minDuration
                                && d <= testConfig.maxDuration
            (sign, pk) <- genBlind sshGen
            user <- jsFactUser testRun $ encodePublicKey pk
            role <- jsFactRole testRun
            let previous = case tryIndex testRun of
                    1 -> []
                    n -> do
                        let previousTestRun = testRun{tryIndex = n - 1}
                        previousState <-
                            Pending (Duration duration)
                                <$> signTestRun
                                    sign
                                    previousTestRun
                        toJSFact previousTestRun previousState
                validation =
                    mkValidation
                        ([user, role] <> previous)
                        [gitCommit testRun]
                        [gitDirectory testRun]
                        []
                        []
            testRunState <-
                Pending (Duration duration)
                    <$> signTestRun sign testRun
            pure $ do
                mresult <-
                    runValidate
                        $ validateCreateTestRunCore
                            testConfig
                            validation
                            testRun
                            testRunState
                mresult `shouldBe` ValidationSuccess Validated

        it "reports unaccaptable duration" $ egenProperty $ do
            duration <- genShrinkA
            testRun <- testRunEGen
            testConfig <- testConfigEGen
            signature <- gen signatureGen
            let testRunState = Pending (Duration duration) signature
            pure $ do
                mresult <-
                    runValidate
                        $ validateCreateTestRunCore
                            testConfig
                            noValidation
                            testRun
                            testRunState
                onConditionHaveReason mresult UnacceptableDuration
                    $ duration < minDuration testConfig
                        || duration > maxDuration testConfig

        it "reports unacceptable role" $ egenProperty $ do
            testConfig <- testConfigEGen
            duration <- genA
            signature <- gen signatureGen
            testRunRequest <- testRunEGen
            testRunFact <-
                gen
                    $ oneof
                        [ changePlatform testRunRequest
                        , changeRequester testRunRequest
                        , changeOrganization testRunRequest
                        , changeProject testRunRequest
                        , pure testRunRequest
                        ]
            role <- jsFactRole testRunFact
            let validation = mkValidation [role] [] [] [] []
                testRunState = Pending (Duration duration) signature
            pure $ do
                mresult <-
                    runValidate
                        $ validateCreateTestRunCore
                            testConfig
                            validation
                            testRunRequest
                            testRunState
                onConditionHaveReason mresult UnacceptableRole
                    $ testRunRequest.platform /= testRunFact.platform
                        || testRunRequest.repository.organization
                            /= testRunFact.repository.organization
                        || testRunRequest.repository.project
                            /= testRunFact.repository.project
                        || testRunRequest.requester /= testRunFact.requester

        it "reports unacceptable try index" $ egenProperty $ do
            testConfig <- testConfigEGen
            duration <- genA
            signature <- gen signatureGen
            testRunR <- testRunEGen
            testRunDB <-
                gen
                    $ oneof
                        [ pure $ tryIndexL .~ 0 $ testRunR
                        , pure testRunR
                        ]
            testRun <-
                gen
                    $ oneof
                        [ changeTry testRunDB
                        , pure $ tryIndexL %~ succ $ testRunDB
                        ]
            let mkTestRunFact :: Monad m => TestRunState x -> m JSFact
                mkTestRunFact = toJSFact testRunDB
                pending = Pending (Duration duration) signature
                accepted = Accepted pending
            rejections <-
                gen $ listOf $ elements [BrokenInstructions, UnclearIntent]
            let rejected = Rejected pending rejections
            finalDuration <- genA
            finalURL <- genA
            let finished =
                    Finished accepted (Duration finalDuration) (URL finalURL)
            testRunStateDB <-
                gen
                    $ oneof
                        [ mkTestRunFact pending
                        , mkTestRunFact accepted
                        , mkTestRunFact rejected
                        , mkTestRunFact finished
                        ]
            testRunFact <- toJSFact testRunDB testRunStateDB
            let validation =
                    mkValidation
                        [testRunFact | testRunDB.tryIndex > 0]
                        []
                        []
                        []
                        []
            let testRunState = Pending (Duration duration) signature
            pure
                $ counterexample (show testRunDB)
                $ cover
                    0.1
                    (testRunDB.tryIndex > 0)
                    "enough stored facts"
                $ cover
                    0.1
                    (testRun.tryIndex == testRunDB.tryIndex + 1)
                    "enough success"
                $ property
                $ do
                    mresult <-
                        runValidate
                            $ validateCreateTestRunCore
                                testConfig
                                validation
                                testRun
                                testRunState
                    onConditionHaveReason mresult UnacceptableTryIndex
                        $ testRun.tryIndex /= testRunDB.tryIndex + 1

        it "reports unacceptable directory" $ egenProperty $ do
            testConfig <- testConfigEGen
            duration <- genA
            signature <- gen signatureGen
            testRun <- testRunEGen
            testRun' <- gen $ oneof [changeDirectory testRun, pure testRun]
            let testRunState = Pending (Duration duration) signature
            testRunFact <- toJSFact testRun' testRunState
            let validation =
                    mkValidation
                        [testRunFact]
                        []
                        [gitDirectory testRun']
                        []
                        []
            pure $ do
                mresult <-
                    runValidate
                        $ validateCreateTestRunCore
                            testConfig
                            validation
                            testRun
                            testRunState
                onConditionHaveReason mresult UnacceptableDirectory
                    $ testRun /= testRun'

module Oracle.Validate.Requests.TestRun.Others
    ( validateToDoneUpdate
    ) where

import Core.Types
    ( Change (..)
    , Key (..)
    , Operation (..)
    , Owner
    , parseFacts
    )
import Oracle.Types (Request (..))
import Oracle.Validate.Types (ValidationResult (..))
import User.Types
    ( Phase (..)
    , TestRun (..)
    , TestRunState (..)
    )
import Validation (Validation (..))

data AgentRejection = PreviousStateWrong
    deriving (Show, Eq)

validateToDoneUpdate
    :: Monad m
    => Owner
    -> Validation m
    -> Request TestRun (TestRunState DoneT)
    -> m ValidationResult
validateToDoneUpdate
    pkh
    validation
    (Request _refId owner (Change (Key testRun) operation)) = do
        if owner /= pkh
            then pure $ NotValidated "request not from agent"
            else case operation of
                Update _ newState -> do
                    result <-
                        validateFromPendingCore
                            validation
                            testRun
                            newState
                    case result of
                        Nothing -> pure Validated
                        Just rejections ->
                            pure
                                $ CannotValidate
                                $ "test run validation failed for the following reasons: "
                                    <> show rejections
                _ ->
                    pure
                        $ CannotValidate
                            "only update operation is supported for test run rejection"

validateFromPendingCore
    :: Monad m
    => Validation m
    -> TestRun
    -> TestRunState DoneT
    -> m (Maybe AgentRejection)
validateFromPendingCore
    validation
    testRun = \case
        Rejected pending _ -> checkPending validation testRun pending
        Finished accepted _ _ -> checkRunning validation testRun accepted

checkRunning
    :: Monad m
    => Validation m
    -> TestRun
    -> TestRunState RunningT
    -> m (Maybe AgentRejection)
checkRunning Validation{mpfsGetFacts} testRun accepted = do
    facts <- mpfsGetFacts
    let testRuns :: [(TestRun, TestRunState RunningT)] = parseFacts facts
    if (testRun, accepted) `elem` testRuns
        then pure Nothing
        else pure $ Just PreviousStateWrong

checkPending
    :: Monad m
    => Validation m
    -> TestRun
    -> TestRunState PendingT
    -> m (Maybe AgentRejection)
checkPending Validation{mpfsGetFacts} testRun pending = do
    facts <- mpfsGetFacts
    let testRuns :: [(TestRun, TestRunState PendingT)] = parseFacts facts
    if (testRun, pending) `elem` testRuns
        then pure Nothing
        else pure $ Just PreviousStateWrong

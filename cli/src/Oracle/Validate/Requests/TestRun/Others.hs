module Oracle.Validate.Requests.TestRun.Others
    ( validateToDoneCore
    , validateToDoneUpdate
    , validateToRunningCore
    , validateToRunningUpdate
    , AgentRejection (..)
    ) where

import Core.Types
    ( Change (..)
    , Key (..)
    , Operation (..)
    , Owner
    )
import Oracle.Types (Request (..))
import Oracle.Validate.Types (ValidationResult (..))
import Text.JSON.Canonical (FromJSON)
import User.Types
    ( Phase (..)
    , TestRun (..)
    , TestRunState (..)
    )
import Validation (Validation (..))

data AgentRejection = PreviousStateWrong
    deriving (Show, Eq)

checkingOwner
    :: Monad m
    => Owner
    -> Owner
    -> m ValidationResult
    -> m ValidationResult
checkingOwner owner pkh f =
    if owner /= pkh
        then pure $ NotValidated "request not from agent"
        else f

checkingUpdates
    :: (Monad m, Show a)
    => Operation t
    -> (t -> m (Maybe a))
    -> m ValidationResult
checkingUpdates operation f = case operation of
    Update _ newState -> do
        result <- f newState
        case result of
            Nothing -> pure Validated
            Just rejection ->
                pure
                    $ CannotValidate
                    $ "test run validation failed for the following reasons: "
                        <> show rejection
    _ ->
        pure
            $ CannotValidate
                "only update operation is supported for test run updates"

validateToDoneUpdate
    :: Monad m
    => Owner
    -> Validation m
    -> Request TestRun (TestRunState DoneT)
    -> m ValidationResult
validateToDoneUpdate
    antiOwner
    validation
    (Request _refId owner (Change (Key testRun) operation)) = do
        checkingOwner owner antiOwner
            $ checkingUpdates operation
            $ validateToDoneCore validation testRun

validateToDoneCore
    :: Monad m
    => Validation m
    -> TestRun
    -> TestRunState DoneT
    -> m (Maybe AgentRejection)
validateToDoneCore
    validation
    testRun = \case
        Rejected pending _ -> checkPastState validation testRun pending
        Finished accepted _ _ -> checkPastState validation testRun accepted

checkPastState
    :: (Monad m, FromJSON Maybe (TestRunState t))
    => Validation m
    -> TestRun
    -> TestRunState t
    -> m (Maybe AgentRejection)
checkPastState Validation{mpfsGetFacts} testRun accepted = do
    testRuns <- mpfsGetFacts
    if (testRun, accepted) `elem` testRuns
        then pure Nothing
        else pure $ Just PreviousStateWrong

validateToRunningUpdate
    :: Monad m
    => Owner
    -> Validation m
    -> Request TestRun (TestRunState RunningT)
    -> m ValidationResult
validateToRunningUpdate
    antiOwner
    validation
    (Request _refId owner (Change (Key testRun) operation)) = do
        checkingOwner owner antiOwner
            $ checkingUpdates operation
            $ validateToRunningCore validation testRun

validateToRunningCore
    :: Monad m
    => Validation m
    -> TestRun
    -> TestRunState RunningT
    -> m (Maybe AgentRejection)
validateToRunningCore
    validation
    testRun = \case
        Accepted pending -> checkPastState validation testRun pending

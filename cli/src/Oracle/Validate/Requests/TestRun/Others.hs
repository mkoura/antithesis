module Oracle.Validate.Requests.TestRun.Others
    ( validateToDoneCore
    , validateToDoneUpdate
    , validateToRunningCore
    , validateToRunningUpdate
    , AgentRejection (..)
    ) where

import Core.Types.Basic (Owner)
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..))
import Core.Types.Operation (Op (..), Operation (..))
import Oracle.Validate.Types (ValidationResult (..))
import Text.JSON.Canonical (FromJSON)
import User.Types
    ( Phase (..)
    , TestRun (..)
    , TestRunState (..)
    )
import Validation (Validation (..), updateValidation)

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
    => Operation (OpU x b)
    -> (b -> m (Maybe a))
    -> m ValidationResult
checkingUpdates operation f = case operation of
    Update _ newState -> do
        result <- f newState
        case result of
            Nothing -> pure Validated
            Just rejection ->
                pure
                    $ NotValidated
                    $ "test run validation failed for the following reasons: "
                        <> show rejection

validateToDoneUpdate
    :: (Monad m, FromJSON Maybe x)
    => Owner
    -> Validation m
    -> Owner
    -> Change TestRun (OpU x (TestRunState DoneT))
    -> m ValidationResult
validateToDoneUpdate
    antiOwner
    validation
    owner
    change@(Change (Key testRun) operation) = do
        updatingValidation <- updateValidation validation change
        if updatingValidation /= Validated
            then pure updatingValidation
            else
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
    if Fact testRun accepted `elem` testRuns
        then pure Nothing
        else pure $ Just PreviousStateWrong

validateToRunningUpdate
    :: Monad m
    => Owner
    -> Validation m
    -> Owner
    -> Change TestRun (OpU (TestRunState PendingT) (TestRunState RunningT))
    -> m ValidationResult
validateToRunningUpdate
    antiOwner
    validation
    owner
    change@(Change (Key testRun) operation) = do
        updatingValidation <- updateValidation validation change
        if updatingValidation /= Validated
            then pure updatingValidation
            else
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

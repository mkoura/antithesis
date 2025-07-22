module Oracle.Validate.Requests.TestRun.Others
    ( validateToDoneCore
    , validateToDoneUpdate
    , validateToRunningCore
    , validateToRunningUpdate
    , AgentRejection (..)
    ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic (Owner)
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..))
import Core.Types.Operation (Op (..), Operation (..))
import Oracle.Validate.Types
    ( Validate
    , ValidationResult
    , mapFailure
    , notValidated
    , runValidate
    )
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
    -> Validate String m ()
checkingOwner owner pkh =
    when (owner /= pkh)
        $ notValidated "request not from agent"

checkingUpdates
    :: (Monad m, Show a)
    => Operation (OpU x b)
    -> (b -> m (Maybe a))
    -> Validate String m ()
checkingUpdates operation f = case operation of
    Update _ newState -> do
        result <- lift $ f newState
        case result of
            Nothing -> pure ()
            Just rejection ->
                notValidated
                    $ "test run validation failed for the following reasons: "
                        <> show rejection

validateToDoneUpdate
    :: (Monad m, FromJSON Maybe x)
    => Owner
    -> Validation m
    -> Owner
    -> Change TestRun (OpU x (TestRunState DoneT))
    -> m (ValidationResult String)
validateToDoneUpdate
    antiOwner
    validation
    owner
    change@(Change (Key testRun) operation) = runValidate $ do
        mapFailure show $ updateValidation validation change
        checkingOwner owner antiOwner
        checkingUpdates operation
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
    -> m (ValidationResult String)
validateToRunningUpdate
    antiOwner
    validation
    owner
    change@(Change (Key testRun) operation) = runValidate $ do
        mapFailure show $ updateValidation validation change
        checkingOwner owner antiOwner
        checkingUpdates operation $ validateToRunningCore validation testRun

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

module Oracle.Validate.Requests.TestRun.Update
    ( validateToDoneCore
    , validateToDoneUpdate
    , validateToRunningCore
    , validateToRunningUpdate
    , AgentRejection (..)
    , UpdateTestRunFailure (..)
    , renderUpdateTestRunFailure
    ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic (Owner)
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..))
import Core.Types.Operation (Op (..), Operation (..))
import Data.Foldable (for_)
import Lib.JSON
import Oracle.Validate.Types
    ( Validate
    , Validated (..)
    , mapFailure
    , notValidated
    )
import Text.JSON.Canonical (FromJSON (..), ToJSON (..))
import User.Types
    ( Phase (..)
    , TestRun (..)
    , TestRunState (..)
    )
import Validation
    ( KeyFailure
    , Validation (..)
    , renderKeyFailure
    , updateValidation
    )

data AgentRejection = PreviousStateWrong
    deriving (Show, Eq)

data UpdateTestRunFailure
    = UpdateTestRunKeyFailure KeyFailure
    | UpdateTestRunAgentRejection AgentRejection
    | UpdateTestRunRequestNotFromAgent Owner
    deriving (Show, Eq)

instance Monad m => ToJSON m UpdateTestRunFailure where
    toJSON = \case
        UpdateTestRunKeyFailure keyFailure ->
            object ["updateTestRunKeyFailure" .= renderKeyFailure keyFailure]
        UpdateTestRunAgentRejection rejection ->
            object ["updateTestRunAgentRejection" .= show rejection]
        UpdateTestRunRequestNotFromAgent owner ->
            object ["updateTestRunRequestNotFromAgent" .= show owner]

renderUpdateTestRunFailure :: UpdateTestRunFailure -> String
renderUpdateTestRunFailure = \case
    UpdateTestRunKeyFailure keyFailure ->
        "Update Test Run Key Failure: " ++ renderKeyFailure keyFailure
    UpdateTestRunAgentRejection rejection ->
        "Update Test Run Agent Rejection: " ++ show rejection
    UpdateTestRunRequestNotFromAgent owner ->
        "Update Test Run Request Not From Agent: " ++ show owner

checkingOwner
    :: Monad m
    => Owner
    -> Owner
    -> Validate UpdateTestRunFailure m ()
checkingOwner owner pkh =
    when (owner /= pkh)
        $ notValidated
        $ UpdateTestRunRequestNotFromAgent owner

checkingUpdates
    :: Monad m
    => Operation (OpU x b)
    -> (b -> m (Maybe AgentRejection))
    -> Validate UpdateTestRunFailure m Validated
checkingUpdates operation f = case operation of
    Update _ newState -> do
        result <- lift $ f newState
        for_ result $ notValidated . UpdateTestRunAgentRejection
        pure Validated

validateToDoneUpdate
    :: (Monad m, FromJSON Maybe x)
    => Owner
    -> Validation m
    -> Owner
    -> Change TestRun (OpU x (TestRunState DoneT))
    -> Validate UpdateTestRunFailure m Validated
validateToDoneUpdate
    antiOwner
    validation
    owner
    change@(Change (Key testRun) operation) = do
        mapFailure UpdateTestRunKeyFailure
            $ updateValidation validation change
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
    -> Validate UpdateTestRunFailure m Validated
validateToRunningUpdate
    antiOwner
    validation
    owner
    change@(Change (Key testRun) operation) = do
        mapFailure UpdateTestRunKeyFailure
            $ updateValidation validation change
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

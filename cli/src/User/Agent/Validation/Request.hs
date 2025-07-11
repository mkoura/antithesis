module User.Agent.Validation.Request
    ( validateRequest
    ) where

import Core.Types
    ( Duration (..)
    , Fact (..)
    , Try (..)
    , parseFacts
    )
import Data.Maybe (catMaybes)
import Text.JSON.Canonical (ToJSON (..))
import User.Agent.Validation.Config (AgentValidationConfig (..))
import User.Types
    ( Phase (PendingT)
    , TestRun (..)
    , TestRunRejection (..)
    , TestRunState (..)
    , roleOfATestRun
    )
import Validation (Validation (..))

checkDuration
    :: AgentValidationConfig -> Duration -> Maybe TestRunRejection
checkDuration AgentValidationConfig{maxDuration, minDuration} (Duration n)
    | n < minDuration || n > maxDuration = Just UnacceptableDuration
    | otherwise = Nothing

checkRole
    :: Monad m => Validation m -> TestRun -> m (Maybe TestRunRejection)
checkRole
    Validation{mpfsGetFacts}
    testRun = do
        fs <- mpfsGetFacts
        let roleFact = roleOfATestRun testRun
        roleFactKey <- toJSON roleFact
        roleFactValue <- toJSON ()
        if Fact roleFactKey roleFactValue `elem` fs
            then return Nothing
            else return $ Just UnacceptableRole

checkTryIndex
    :: Monad m
    => Validation m
    -> TestRun
    -> m (Maybe TestRunRejection)
checkTryIndex
    Validation{mpfsGetFacts}
    testRun = do
        fs <- mpfsGetFacts
        let testRuns :: [(TestRun, TestRunState PendingT)] = parseFacts fs
        let sameCommitTestRuns =
                filter
                    ( \(tr, _) ->
                        repository tr == repository testRun
                            && commitId tr == commitId testRun
                            && platform tr == platform testRun
                            && directory tr == directory testRun
                    )
                    testRuns
            latest = case sameCommitTestRuns of
                [] -> Try 0
                _ -> maximum $ map (tryIndex . fst) sameCommitTestRuns
        if tryIndex testRun == succ latest
            then return Nothing
            else return $ Just UnacceptableTryIndex

validateRequest
    :: Monad m
    => AgentValidationConfig
    -> Validation m
    -> TestRun
    -> TestRunState PendingT
    -> m (Maybe [TestRunRejection])
validateRequest
    config
    validation
    testRun
    (Pending duration) = do
        result <-
            catMaybes
                <$> sequence
                    [ pure $ checkDuration config duration
                    , checkRole validation testRun
                    , checkTryIndex validation testRun
                    ]

        case result of
            [] -> return Nothing
            reasons -> return $ Just reasons

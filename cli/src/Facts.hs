module Facts
    ( FactsSelection (..)
    , TestRunSelection (..)
    , factsCmd
    )
where

import Control.Monad (filterM)
import Core.Types.Basic (TokenId)
import Core.Types.Fact (Fact (..), keyHash, parseFacts)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import MPFS.API (MPFS, mpfsGetTokenFacts)
import Oracle.Config.Types (Config, ConfigKey)
import Text.JSON.Canonical
import User.Agent.Types (TestRunId (..), WhiteListKey)
import User.Types
    ( Phase (..)
    , RegisterUserKey
    , TestRun
    , TestRunState (..)
    )

data TestRunSelection a where
    TestRunPending
        :: [TestRunId]
        -> TestRunSelection [Fact TestRun (TestRunState 'PendingT)]
    TestRunRunning
        :: [TestRunId]
        -> TestRunSelection [Fact TestRun (TestRunState 'RunningT)]
    TestRunDone
        :: [TestRunId]
        -> TestRunSelection [Fact TestRun (TestRunState 'DoneT)]
    TestRunRejected
        :: [TestRunId]
        -> TestRunSelection [Fact TestRun (TestRunState 'DoneT)]
data FactsSelection a where
    UserFacts :: FactsSelection [Fact RegisterUserKey ()]
    RoleFacts :: FactsSelection [Fact RegisterUserKey ()]
    TestRunFacts :: TestRunSelection a -> FactsSelection a
    ConfigFact :: FactsSelection [Fact ConfigKey Config]
    WhiteListedFacts :: FactsSelection [Fact WhiteListKey ()]
    AllFacts :: FactsSelection [Fact JSValue JSValue]

retrieveAnyFacts
    :: (FromJSON Maybe k, FromJSON Maybe v, Functor m)
    => MPFS m
    -> TokenId
    -> m [Fact k v]
retrieveAnyFacts mpfs tokenId = parseFacts <$> mpfsGetTokenFacts mpfs tokenId

filterFacts
    :: (Foldable t, ToJSON Identity k)
    => t TestRunId
    -> [Fact k v]
    -> [Fact k v]
filterFacts ids
    | null ids = id
    | otherwise =
        runIdentity
            . filterM (\v -> (`elem` ids) . TestRunId <$> keyHash (factKey v))

factsCmd :: Monad m => MPFS m -> TokenId -> FactsSelection a -> m a
factsCmd mpfs tokenId UserFacts = retrieveAnyFacts mpfs tokenId
factsCmd mpfs tokenId RoleFacts = retrieveAnyFacts mpfs tokenId
factsCmd mpfs tokenId (TestRunFacts (TestRunPending ids)) = do
    retrieveAnyFacts mpfs tokenId <&> filterFacts ids
factsCmd mpfs tokenId (TestRunFacts (TestRunRunning ids)) =
    retrieveAnyFacts mpfs tokenId <&> filterFacts ids
factsCmd mpfs tokenId (TestRunFacts (TestRunDone ids)) = do
    facts <- retrieveAnyFacts mpfs tokenId <&> filterFacts ids
    pure
        $ filter
            ( \v -> case factValue v of
                Finished{} -> True
                _ -> False
            )
            facts
factsCmd mpfs tokenId (TestRunFacts (TestRunRejected ids)) = do
    facts <- retrieveAnyFacts mpfs tokenId <&> filterFacts ids
    pure
        $ filter
            ( \v -> case factValue v of
                Rejected{} -> True
                _ -> False
            )
            facts
factsCmd mpfs tokenId ConfigFact = retrieveAnyFacts mpfs tokenId
factsCmd mpfs tokenId WhiteListedFacts = retrieveAnyFacts mpfs tokenId
factsCmd mpfs tokenId AllFacts = retrieveAnyFacts mpfs tokenId

module Facts
    ( FactsSelection (..)
    , TestRunSelection (..)
    , factsCmd
    )
where

import Core.Types.Basic (TokenId)
import Core.Types.Fact (Fact (..), parseFacts)
import MPFS.API (MPFS, mpfsGetTokenFacts)
import Oracle.Config.Types (Config, ConfigKey)
import Text.JSON.Canonical
import User.Agent.Types (WhiteListKey)
import User.Types
    ( Phase (..)
    , RegisterUserKey
    , TestRun
    , TestRunState (..)
    )

data TestRunSelection a where
    TestRunPending
        :: TestRunSelection [Fact TestRun (TestRunState 'PendingT)]
    TestRunRunning
        :: TestRunSelection [Fact TestRun (TestRunState 'RunningT)]
    TestRunDone :: TestRunSelection [Fact TestRun (TestRunState 'DoneT)]
    TestRunRejected
        :: TestRunSelection [Fact TestRun (TestRunState 'DoneT)]
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

factsCmd :: Monad m => MPFS m -> TokenId -> FactsSelection a -> m a
factsCmd mpfs tokenId UserFacts = retrieveAnyFacts mpfs tokenId
factsCmd mpfs tokenId RoleFacts = retrieveAnyFacts mpfs tokenId
factsCmd mpfs tokenId (TestRunFacts TestRunPending) = retrieveAnyFacts mpfs tokenId
factsCmd mpfs tokenId (TestRunFacts TestRunRunning) = retrieveAnyFacts mpfs tokenId
factsCmd mpfs tokenId (TestRunFacts TestRunDone) = do
    facts <- retrieveAnyFacts mpfs tokenId
    pure
        $ filter
            ( \v -> case factValue v of
                Finished{} -> True
                _ -> False
            )
            facts
factsCmd mpfs tokenId (TestRunFacts TestRunRejected) = do
    facts <- retrieveAnyFacts mpfs tokenId
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

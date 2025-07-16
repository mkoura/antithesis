module User.Agent.Types
    ( TestRunStatus (..)
    , TestRunMap (..)
    ) where

import Core.Types (Fact)
import Lib.JSON (object, (.=))
import Text.JSON.Canonical (ToJSON (..))
import User.Types (Phase (..), TestRun, TestRunState)

data TestRunStatus phase where
    StatusPending
        :: Fact TestRun (TestRunState PendingT) -> TestRunStatus PendingT
    StatusRunning
        :: Fact TestRun (TestRunState RunningT) -> TestRunStatus RunningT
    StatusDone :: Fact TestRun (TestRunState DoneT) -> TestRunStatus DoneT

instance Monad m => ToJSON m (TestRunStatus phase) where
    toJSON (StatusPending fact) = toJSON fact
    toJSON (StatusRunning fact) = toJSON fact
    toJSON (StatusDone fact) = toJSON fact

data TestRunMap = TestRunMap
    { pending :: [TestRunStatus PendingT]
    , running :: [TestRunStatus RunningT]
    , done :: [TestRunStatus DoneT]
    }

instance Monad m => ToJSON m TestRunMap where
    toJSON (TestRunMap p r d) =
        object
            [ "pending" .= p
            , "running" .= r
            , "done" .= d
            ]

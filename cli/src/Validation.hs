module Validation
    ( Validation (..)
    ) where

import Core.Types (Commit, Fact, Repository)

-- | Abstract the side effects necessary for validation.
data Validation m = Validation
    { mpfsGetFacts :: m [Fact]
    , githubCommitExists :: Repository -> Commit -> m Bool
    }

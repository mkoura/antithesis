module Validation
    ( Validation (..)
    , mkValidation
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Core.Types.Basic (Commit, Directory, Repository, TokenId)
import Core.Types.Fact (Fact (..), JSFact, parseFacts)
import Data.Maybe (mapMaybe)
import Lib.GitHub qualified as GitHub
import MPFS.API (getTokenFacts)
import Servant.Client (ClientM)
import Text.JSON.Canonical (FromJSON (..))
import User.Types (TestRun)

-- | Abstract the side effects necessary for validation.
data Validation m = Validation
    { mpfsGetFacts
        :: forall k v
         . (FromJSON Maybe k, FromJSON Maybe v)
        => m [Fact k v]
    , mpfsGetTestRuns :: m [TestRun]
    , githubCommitExists :: Repository -> Commit -> m Bool
    , githubDirectoryExists :: Repository -> Commit -> Directory -> m Bool
    }

mkValidation :: TokenId -> Validation ClientM
mkValidation tk =
    Validation
        { mpfsGetFacts = parseFacts <$> getTokenFacts tk
        , mpfsGetTestRuns = do
            facts <- parseFacts <$> getTokenFacts tk
            pure $ mapMaybe (\(Fact k _ :: JSFact) -> fromJSON k) facts
        , githubCommitExists = \repository commit ->
            liftIO $ GitHub.githubCommitExists repository commit
        , githubDirectoryExists = \repository commit dir ->
            liftIO $ GitHub.githubDirectoryExists repository commit dir
        }

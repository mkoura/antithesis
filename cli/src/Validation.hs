module Validation
    ( Validation (..)
    , mkValidation
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Core.Types.Basic (Commit, Directory, Repository, TokenId)
import Core.Types.Fact (Fact, parseFacts)
import Lib.GitHub qualified as GitHub
import MPFS.API (getTokenFacts)
import Servant.Client (ClientM)
import Text.JSON.Canonical (FromJSON (..))

-- | Abstract the side effects necessary for validation.
data Validation m = Validation
    { mpfsGetFacts
        :: forall k v
         . (FromJSON Maybe k, FromJSON Maybe v)
        => m [Fact k v]
    , githubCommitExists :: Repository -> Commit -> m Bool
    , githubDirectoryExists :: Repository -> Commit -> Directory -> m Bool
    }

mkValidation :: TokenId -> Validation ClientM
mkValidation tk =
    Validation
        { mpfsGetFacts = do
            factsObject <- getTokenFacts tk
            case fromJSON factsObject of
                Nothing -> error "Failed to parse facts from JSON"
                Just factsObject' -> pure $ parseFacts factsObject'
        , githubCommitExists = \repository commit ->
            liftIO $ GitHub.githubCommitExists repository commit
        , githubDirectoryExists = \repository commit dir ->
            liftIO $ GitHub.githubDirectoryExists repository commit dir
        }

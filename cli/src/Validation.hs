module Validation
    ( Validation (..)
    , mkValidation
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Core.Types
    ( Commit
    , Directory
    , Fact (..)
    , Repository
    , TokenId
    , parseFacts
    )
import Lib.GitHub qualified as GitHub
import MPFS.API (getTokenFacts)
import Servant.Client (ClientM)
import Text.JSON.Canonical (FromJSON (..))

-- | Abstract the side effects necessary for validation.
data Validation m = Validation
    { mpfsGetFacts :: m [Fact]
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
                Just factsObject' -> do
                    let factsList = parseFacts factsObject'
                    return $ uncurry Fact <$> factsList
        , githubCommitExists = \repository commit ->
            liftIO $ GitHub.githubCommitExists repository commit
        , githubDirectoryExists = \repository commit dir ->
            liftIO $ GitHub.githubDirectoryExists repository commit dir
        }

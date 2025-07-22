{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unless" #-}
module Validation
    ( Validation (..)
    , KeyFailure (..)
    , mkValidation
    , insertValidation
    , deleteValidation
    , updateValidation
    , renderKeyFailure
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic
    ( Commit
    , Directory
    , PublicKeyHash
    , Repository
    , TokenId
    , Username
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..), JSFact, parseFacts)
import Core.Types.Operation (Op (..))
import Data.Maybe (mapMaybe)
import Lib.GitHub qualified as GitHub
import MPFS.API (getTokenFacts)
import Oracle.Validate.Types (Validate, notValidated)
import Servant.Client (ClientM)
import Text.JSON.Canonical (FromJSON (..))
import User.Types (TestRun)
import Validation.RegisterRole
    ( RepositoryRoleFailure
    , inspectRepoRoleForUser
    )
import Validation.RegisterUser
    ( PublicKeyFailure
    , inspectPublicKey
    )

-- | Abstract the side effects necessary for validation.
data Validation m = Validation
    { mpfsGetFacts
        :: forall k v
         . (FromJSON Maybe k, FromJSON Maybe v)
        => m [Fact k v]
    , mpfsGetTestRuns :: m [TestRun]
    , githubCommitExists :: Repository -> Commit -> m Bool
    , githubDirectoryExists :: Repository -> Commit -> Directory -> m Bool
    , githubUserPublicKeys
        :: Username
        -> PublicKeyHash
        -> m (Maybe PublicKeyFailure)
    , githubRepositoryRole
        :: Username
        -> Repository
        -> m (Maybe RepositoryRoleFailure)
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
        , githubUserPublicKeys = \username publicKey ->
            liftIO $ inspectPublicKey username publicKey
        , githubRepositoryRole = \username repository ->
            liftIO $ inspectRepoRoleForUser username repository
        }
data KeyFailure
    = KeyAlreadyExists String
    | KeyDoesNotExist String
    deriving (Show, Eq)

renderKeyFailure :: KeyFailure -> String
renderKeyFailure = \case
    KeyAlreadyExists key -> "Key already exists: " <> key
    KeyDoesNotExist key -> "Key does not exist: " <> key

-- | Validate a change just as an mpf change.
-- * Insert should have a fresh key
-- * Update should have a key that exists
-- * Delete should have a key that exists
insertValidation
    :: forall m k v
     . (Monad m, FromJSON Maybe k, Eq k, Show k, FromJSON Maybe v)
    => Validation m
    -> Change k (OpI v)
    -> Validate KeyFailure m ()
insertValidation Validation{mpfsGetFacts} (Change (Key k) _) = do
    facts :: [Fact k v] <- lift mpfsGetFacts
    when (any (\(Fact k' _) -> k' == k) facts)
        $ notValidated
        $ KeyAlreadyExists
        $ show k

deleteValidation
    :: forall m k v
     . (Monad m, FromJSON Maybe k, Eq k, Show k, FromJSON Maybe v)
    => Validation m
    -> Change k (OpD v)
    -> Validate KeyFailure m ()
deleteValidation Validation{mpfsGetFacts} (Change (Key k) _) = do
    facts :: [Fact k v] <- lift mpfsGetFacts
    when (not $ any (\(Fact k' _) -> k' == k) facts)
        $ notValidated
        $ KeyDoesNotExist
        $ show k

updateValidation
    :: forall m k v w
     . (Monad m, FromJSON Maybe k, Eq k, Show k, FromJSON Maybe v)
    => Validation m
    -> Change k (OpU v w)
    -> Validate KeyFailure m ()
updateValidation Validation{mpfsGetFacts} (Change (Key k) _) = do
    facts :: [Fact k v] <- lift mpfsGetFacts
    when (not $ any (\(Fact k' _) -> k' == k) facts)
        $ notValidated
        $ KeyDoesNotExist
        $ show k

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
    , hoistValidation
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
import GitHub (Auth)
import Lib.GitHub qualified as GitHub
import MPFS.API (getTokenFacts)
import Oracle.Validate.Types (Validate, Validated (..), notValidated)
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
    , githubCommitExists
        :: Repository
        -> Commit
        -> m (Either GitHub.GithubResponseError Bool)
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

hoistValidation
    :: (forall a. m a -> n a)
    -> Validation m
    -> Validation n
hoistValidation
    f
    Validation
        { mpfsGetFacts
        , mpfsGetTestRuns
        , githubCommitExists
        , githubDirectoryExists
        , githubUserPublicKeys
        , githubRepositoryRole
        } =
        Validation
            { mpfsGetFacts = f mpfsGetFacts
            , mpfsGetTestRuns = f mpfsGetTestRuns
            , githubCommitExists =
                \repo commit -> f $ githubCommitExists repo commit
            , githubDirectoryExists =
                \repo commit dir -> f $ githubDirectoryExists repo commit dir
            , githubUserPublicKeys =
                \username publicKey -> f $ githubUserPublicKeys username publicKey
            , githubRepositoryRole =
                \username repository -> f $ githubRepositoryRole username repository
            }

mkValidation :: Auth -> TokenId -> Validation ClientM
mkValidation auth tk =
    Validation
        { mpfsGetFacts = parseFacts <$> getTokenFacts tk
        , mpfsGetTestRuns = do
            facts <- parseFacts <$> getTokenFacts tk
            pure $ mapMaybe (\(Fact k _ :: JSFact) -> fromJSON k) facts
        , githubCommitExists = \repository commit ->
            liftIO $ GitHub.githubCommitExists auth repository commit
        , githubDirectoryExists = \repository commit dir ->
            liftIO $ GitHub.githubDirectoryExists auth repository commit dir
        , githubUserPublicKeys = \username publicKey ->
            liftIO $ inspectPublicKey auth username publicKey
        , githubRepositoryRole = \username repository ->
            liftIO $ inspectRepoRoleForUser auth username repository
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
    -> Validate KeyFailure m Validated
deleteValidation Validation{mpfsGetFacts} (Change (Key k) _) = do
    facts :: [Fact k v] <- lift mpfsGetFacts
    when (not $ any (\(Fact k' _) -> k' == k) facts)
        $ notValidated
        $ KeyDoesNotExist
        $ show k
    pure Validated

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

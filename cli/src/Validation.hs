{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unless" #-}
module Validation
    ( Validation (..)
    , KeyFailure (..)
    , mkValidation
    , insertValidation
    , deleteValidation
    , updateValidation
    , hoistValidation
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic
    ( Commit
    , Directory
    , FileName
    , PublicKeyHash
    , Repository
    , TokenId
    , Username
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..), JSFact, parseFacts)
import Core.Types.Operation (Op (..))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GitHub (Auth)
import Lib.GitHub qualified as GitHub
import Lib.JSON.Canonical.Extra (object, (.=))
import MPFS.API (getToken, getTokenFacts)
import Oracle.Types (RequestZoo, Token (tokenRequests))
import Oracle.Validate.Types (Validate, notValidated)
import Servant.Client (ClientM)
import Text.JSON.Canonical (FromJSON (..), ToJSON)
import Text.JSON.Canonical.Class (ToJSON (..))
import User.Types (TestRun)
import Validation.DownloadFile
    ( DownloadedFileFailure
    , inspectDownloadedFile
    )
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
    , mpfsGetTokenRequests :: m [RequestZoo]
    , githubCommitExists
        :: Repository
        -> Commit
        -> m (Either GitHub.GithubResponseError Bool)
    , githubDirectoryExists
        :: Repository
        -> Commit
        -> Directory
        -> m (Either GitHub.GithubResponseStatusCodeError Bool)
    , githubUserPublicKeys
        :: Username
        -> PublicKeyHash
        -> m (Maybe PublicKeyFailure)
    , githubRepositoryExists
        :: Repository
        -> m (Either GitHub.GithubResponseStatusCodeError Bool)
    , githubRepositoryRole
        :: Username
        -> Repository
        -> m (Maybe RepositoryRoleFailure)
    , githubGetFile
        :: Repository
        -> Maybe Commit
        -> FileName
        -> m (Either DownloadedFileFailure Text)
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
        , mpfsGetTokenRequests
        , githubCommitExists
        , githubDirectoryExists
        , githubUserPublicKeys
        , githubRepositoryExists
        , githubRepositoryRole
        , githubGetFile
        } =
        Validation
            { mpfsGetFacts = f mpfsGetFacts
            , mpfsGetTestRuns = f mpfsGetTestRuns
            , mpfsGetTokenRequests = f mpfsGetTokenRequests
            , githubCommitExists =
                \repo commit -> f $ githubCommitExists repo commit
            , githubDirectoryExists =
                \repo commit dir -> f $ githubDirectoryExists repo commit dir
            , githubUserPublicKeys =
                \username publicKey -> f $ githubUserPublicKeys username publicKey
            , githubRepositoryExists =
                f . githubRepositoryExists
            , githubRepositoryRole =
                \username repository -> f $ githubRepositoryRole username repository
            , githubGetFile =
                \repository commit filename -> f $ githubGetFile repository commit filename
            }

mkValidation :: Auth -> Maybe TokenId -> Validation ClientM
mkValidation auth tk = do
    let getFacts :: (FromJSON Maybe k, FromJSON Maybe v) => ClientM [Fact k v]
        getFacts = maybe (pure []) (fmap parseFacts . getTokenFacts) tk
    Validation
        { mpfsGetFacts = getFacts
        , mpfsGetTestRuns =
            mapMaybe (\(Fact k _ :: JSFact) -> fromJSON k) <$> getFacts
        , mpfsGetTokenRequests = case tk of
            Nothing -> pure []
            Just tokenId -> do
                mtoken <- fromJSON <$> getToken tokenId
                pure $ maybe [] (fmap runIdentity . tokenRequests) mtoken
        , githubCommitExists = \repository commit ->
            liftIO $ GitHub.githubCommitExists auth repository commit
        , githubDirectoryExists = \repository commit dir ->
            liftIO $ GitHub.githubDirectoryExists auth repository commit dir
        , githubUserPublicKeys = \username publicKey ->
            liftIO $ inspectPublicKey auth username publicKey
        , githubRepositoryExists = liftIO . GitHub.githubRepositoryExists auth
        , githubRepositoryRole = \username repository ->
            liftIO $ inspectRepoRoleForUser auth username repository
        , githubGetFile = \repository commit filename ->
            liftIO $ inspectDownloadedFile auth repository commit filename
        }

data KeyFailure
    = KeyAlreadyExists String
    | KeyDoesNotExist String
    deriving (Show, Eq)

instance Monad m => ToJSON m KeyFailure where
    toJSON = \case
        KeyAlreadyExists key -> object ["keyAlreadyExists" .= key]
        KeyDoesNotExist key -> object ["keyDoesNotExist" .= key]

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
    when (all (\(Fact k' _) -> k' /= k) facts)
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

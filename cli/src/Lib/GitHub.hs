module Lib.GitHub
    ( GithubResponseError (..)
    , GetCodeOwnersFileFailure (..)
    , githubCommitExists
    , githubDirectoryExists
    , githubUserPublicKeys
    , githubGetCodeOwnersFile
    ) where

import Control.Exception
    ( Exception
    , throwIO
    )
import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , Repository (..)
    , Username (..)
    )
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as B
import Data.Foldable (Foldable (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GitHub (Auth (..), FetchCount (..), GitHubRW, github)
import GitHub qualified as GH
import GitHub.Data.Name (Name (..))
import Network.HTTP.Client
    ( HttpException (..)
    , HttpExceptionContent (StatusCodeException)
    , Response (..)
    )
import Network.HTTP.Types (Status (..))
import System.Environment (getEnv)

getOAUth :: IO Auth
getOAUth = do
    tk <- B.pack <$> getEnv "GITHUB_PERSONAL_ACCESS_TOKEN"
    return $ OAuth tk

callGithub :: GitHubRW req (IO b) => req -> IO b
callGithub req = do
    auth <- getOAUth
    github auth req

data GithubResponseError =
      GithubResponseErrorRepositoryNotFound
    | GithubResponseErrorSSHPublicKeysCannotBeFetched String
    deriving (Show)

instance Exception GithubResponseError

-- | Handle http exceptions from GitHub API calls based on the status code.
onStatusCodeOfException :: GH.Error -> (Int -> IO (Maybe a)) -> IO a
onStatusCodeOfException e f = case e of
    GH.HTTPError
        ( HttpExceptionRequest
                _
                (StatusCodeException response _)
            ) -> case responseStatus response of
            Status c _ -> do
                r <- f c
                case r of
                    Just a -> return a
                    Nothing -> throwIO e
    _ -> throwIO e

-- | Check if a commit exists in a GitHub repository.
githubCommitExists :: Repository -> Commit -> IO (Either GithubResponseError Bool)
githubCommitExists (Repository owner repo) (Commit sha) = do
    commit <-
        callGithub
            $ GH.commitR
                owner'
                repo'
                sha'
    case commit of
        Left e -> onStatusCodeOfException e $ \c -> do
            case c of
                404 -> return $ Just $ Left GithubResponseErrorRepositoryNotFound
                422 -> return $ Just $ Right False
                _ -> return Nothing
        Right _ -> return $ Right True
  where
    owner' = N $ T.pack owner
    repo' = N $ T.pack repo
    sha' = N $ T.pack sha

githubDirectoryExists
    :: Repository -> Commit -> Directory -> IO Bool
githubDirectoryExists (Repository owner repo) (Commit sha) (Directory dir) = do
    let path = T.pack dir
    contents <-
        callGithub
            $ GH.contentsForR
                owner'
                repo'
                path
                (Just sha')
    case contents of
        Left e -> onStatusCodeOfException e $ \_ -> do
            return $ Just False
        Right _ -> return True
  where
    owner' = N $ T.pack owner
    repo' = N $ T.pack repo
    sha' = T.pack sha

githubUserPublicKeys :: Username -> IO (Either GithubResponseError [T.Text])
githubUserPublicKeys (Username name) = do
    auth <- getOAUth
    result <-
        github auth $ GH.publicSSHKeysForR (N $ T.pack name) FetchAll
    case result of
        Left e ->
            pure
            $ Left
            $ GithubResponseErrorSSHPublicKeysCannotBeFetched
            $ show e
        Right r -> pure $ Right $ GH.basicPublicSSHKeyKey <$> toList r

data GetCodeOwnersFileFailure
    = GetCodeOwnersFileDirectoryNotFound
    | GetCodeOwnersFileNotAFile
    | GetCodeOwnersFileUnsupportedEncoding String
    | GetCodeOwnersFileOtherFailure String
    deriving (Eq, Show)

instance Exception GetCodeOwnersFileFailure

githubGetCodeOwnersFile
    :: Repository -> IO (Either GetCodeOwnersFileFailure T.Text)
githubGetCodeOwnersFile (Repository owner repo) = do
    response <-
        callGithub
            $ GH.contentsForR
                owner'
                repo'
                "CODEOWNERS"
                Nothing
    case response of
        Left e -> onStatusCodeOfException e $ \c -> do
            case c of
                404 ->
                    pure
                        . Just
                        . Left
                        $ GetCodeOwnersFileDirectoryNotFound
                _ ->
                    pure
                        . Just
                        . Left
                        . GetCodeOwnersFileOtherFailure
                        $ show e
        Right (GH.ContentFile contents) -> do
            let content = GH.contentFileContent contents
            case GH.contentFileEncoding contents of
                "base64" ->
                    pure
                        . Right
                        . T.decodeUtf8
                        . B64.decodeLenient
                        . T.encodeUtf8
                        $ content
                enc ->
                    pure
                        . Left
                        . GetCodeOwnersFileUnsupportedEncoding
                        $ T.unpack enc
        Right _ ->
            pure
                . Left
                $ GetCodeOwnersFileNotAFile
  where
    owner' = N $ T.pack owner
    repo' = N $ T.pack repo

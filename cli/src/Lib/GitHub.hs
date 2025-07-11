module Lib.GitHub
    ( githubCommitExists
    ) where

import Control.Exception
    ( Exception
    , throwIO
    )
import Core.Types (Commit (..), Repository (..))
import Data.ByteString.Char8 qualified as B
import Data.Text qualified as T
import GitHub (Auth (..), GitHubRW, github)
import GitHub.Data.Name (Name (..))
import GitHub.Endpoints.Repos.Commits qualified as GH
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

data GithubError = RepositoryNotFound
    deriving (Show)

instance Exception GithubError

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
githubCommitExists :: Repository -> Commit -> IO Bool
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
                404 -> throwIO RepositoryNotFound
                422 -> return $ Just False
                _ -> return Nothing
        Right _ -> return True
  where
    owner' = N $ T.pack owner
    repo' = N $ T.pack repo
    sha' = N $ T.pack sha

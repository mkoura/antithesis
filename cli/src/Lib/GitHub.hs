module Lib.GitHub
    ( GithubResponseError (..)
    , GetCodeOwnersFileFailure (..)
    , githubCommitExists
    , githubDirectoryExists
    , githubUserPublicKeys
    , githubGetCodeOwnersFile
    , getOAUth
    ) where

import Control.Exception
    ( Exception
    )
import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , Repository (..)
    , Username (..)
    )
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (Foldable (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GitHub (Auth (..), FetchCount (..), github)
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
    tk <- BC.pack <$> getEnv "GITHUB_PERSONAL_ACCESS_TOKEN"
    return $ OAuth tk

data GithubResponseError
    = GithubResponseErrorRepositoryNotFound
    | GithubResponseErrorSSHPublicKeysCannotBeFetched String
    | GithubResponseCodeError GithubResponseStatusCodeError
    deriving (Eq, Show)

instance Exception GithubResponseError

data GithubResponseStatusCodeError
    = GithubResponseStatusCodeNotHandledInClient String
    | GithubResponseStatusCodeNotHTTPError String
    deriving (Eq, Show)

instance Exception GithubResponseStatusCodeError

-- | Handle http exceptions from GitHub API calls based on the status code.
onStatusCodeOfException :: GH.Error -> (Int -> IO (Maybe a)) -> IO (Either GithubResponseStatusCodeError a)
onStatusCodeOfException e f = case e of
    GH.HTTPError
        ( HttpExceptionRequest
                _
                (StatusCodeException response _)
            ) -> case responseStatus response of
            Status c _ -> do
                r <- f c
                case r of
                    Just a -> return $ Right a
                    Nothing ->
                        return
                        $ Left
                        $ GithubResponseStatusCodeNotHandledInClient
                        $ show c
    _ -> return $ Left $ GithubResponseStatusCodeNotHTTPError $ show e

-- | Check if a commit exists in a GitHub repository.
githubCommitExists
    :: Auth -> Repository -> Commit -> IO (Either GithubResponseError Bool)
githubCommitExists auth (Repository owner repo) (Commit sha) = do
    commit <-
        github auth
            $ GH.commitR
                owner'
                repo'
                sha'
    case commit of
        Left e -> do
            res <- onStatusCodeOfException e $ \c -> do
                case c of
                    404 -> return $ Just $ Left GithubResponseErrorRepositoryNotFound
                    422 -> return $ Just $ Right False
                    _ -> return Nothing
            case res of
                Left err -> return $ Left $ GithubResponseCodeError err
                Right a  ->  return a
        Right _ -> return $ Right True
  where
    owner' = N $ T.pack owner
    repo' = N $ T.pack repo
    sha' = N $ T.pack sha

githubDirectoryExists
    :: Auth -> Repository -> Commit -> Directory -> IO Bool
githubDirectoryExists auth (Repository owner repo) (Commit sha) (Directory dir) = do
    let path = T.pack dir
    contents <-
        github auth
            $ GH.contentsForR
                owner'
                repo'
                path
                (Just sha')
    case contents of
        Left e -> do
            res <- onStatusCodeOfException e $ \_ -> do
                return $ Just False
            case res of
                Left err -> undefined -- return $ Left $ GithubResponseCodeError err
                Right a  ->  return a
        Right _ -> return True
  where
    owner' = N $ T.pack owner
    repo' = N $ T.pack repo
    sha' = T.pack sha

githubUserPublicKeys
    :: Auth -> Username -> IO (Either GithubResponseError [T.Text])
githubUserPublicKeys auth (Username name) = do
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
    | GetCodeOwnersFileCodeError GithubResponseStatusCodeError
    deriving (Eq, Show)

instance Exception GetCodeOwnersFileFailure

githubGetCodeOwnersFile
    :: Auth -> Repository -> IO (Either GetCodeOwnersFileFailure T.Text)
githubGetCodeOwnersFile auth (Repository owner repo) = do
    response <-
        github auth
            $ GH.contentsForR
                owner'
                repo'
                "CODEOWNERS"
                Nothing
    case response of
        Left e -> do
            res <- onStatusCodeOfException e $ \c -> do
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
            case res of
                Left err -> return $ Left $ GetCodeOwnersFileCodeError err
                Right a  ->  return a
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

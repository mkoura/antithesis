{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Validation.DownloadFile
    ( DownloadFileFailure (..)
    , inspectDownloadedFileTemplate
    , inspectDownloadedFile
    , renderDownloadedFileFailure
    ) where

import Core.Types.Basic (Commit, Repository, FileName (..))
import Data.Text (Text)
import GitHub (Auth)
import Lib.GitHub (GetGithubFileFailure, githubGetFile)

data DownloadFileFailure
    = GithubGetError GetGithubFileFailure
    deriving (Eq, Show)

renderDownloadedFileFailure :: DownloadFileFailure -> String
renderDownloadedFileFailure = \case
    GithubGetError failure ->
        "Error when interacting with github. Details: " <> show failure

analyzeDownloadedFile
    :: FileName
    -> Either GetGithubFileFailure Text
    -> Maybe DownloadFileFailure
analyzeDownloadedFile (FileName filename) = \case
    Left failure ->
        Just $ GithubGetError failure
    Right _file -> undefined

inspectDownloadedFileTemplate
    :: Repository
    -> Maybe Commit
    -> FileName
    -> (Repository -> Maybe Commit -> FileName -> IO (Either GetGithubFileFailure Text))
    -> IO (Maybe DownloadFileFailure)
inspectDownloadedFileTemplate repo commit filename downloadFile = do
    resp <- downloadFile repo commit filename
    pure $ analyzeDownloadedFile filename resp

inspectDownloadedFile
    :: Auth
    -> Repository
    -> Commit
    -> FileName
    -> IO (Maybe DownloadFileFailure)
inspectDownloadedFile auth repo commit filename =
    inspectDownloadedFileTemplate
        repo
        (Just commit)
        filename
        $ githubGetFile auth

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Validation.DownloadFile
    ( DownloadedFileFailure (..)
    , inspectDownloadedFileTemplate
    , inspectDownloadedFile
    , renderDownloadedFileFailure
    ) where

import Core.Types.Basic (Commit, Repository, FileName (..))
import Data.Text (Text)
import GitHub (Auth)
import Lib.GitHub (GetGithubFileFailure, githubGetFile)

import qualified Data.Text as T
import qualified Text.MMark as MMark

data DownloadedFileFailure
    = GithubGetError GetGithubFileFailure
    | DownloadedFileParseError String
    | DownloadedFileNotSupported
    deriving (Eq, Show)

renderDownloadedFileFailure :: DownloadedFileFailure -> String
renderDownloadedFileFailure = \case
    GithubGetError failure ->
        "Error when interacting with github. Details: " <> show failure
    DownloadedFileParseError failure ->
        "The downloaded file seems to have pare error. Details: " <> show failure
    DownloadedFileNotSupported ->
        "Only `md` and `yaml` files are currently supported in validation"

analyzeDownloadedFile
    :: FileName
    -> Either GetGithubFileFailure Text
    -> Maybe DownloadedFileFailure
analyzeDownloadedFile (FileName filename) = \case
    Left failure ->
        Just $ GithubGetError failure
    Right file ->
        if T.isSuffixOf "md" (T.pack filename) then
            case MMark.parse filename file of
                Left bundle ->
                    Just $ DownloadedFileParseError $ show bundle
                Right _ ->
                    Nothing
        else
            Just DownloadedFileNotSupported

inspectDownloadedFileTemplate
    :: Repository
    -> Maybe Commit
    -> FileName
    -> (Repository -> Maybe Commit -> FileName -> IO (Either GetGithubFileFailure Text))
    -> IO (Maybe DownloadedFileFailure)
inspectDownloadedFileTemplate repo commit filename downloadFile = do
    resp <- downloadFile repo commit filename
    pure $ analyzeDownloadedFile filename resp

inspectDownloadedFile
    :: Auth
    -> Repository
    -> Commit
    -> FileName
    -> IO (Maybe DownloadedFileFailure)
inspectDownloadedFile auth repo commit filename =
    inspectDownloadedFileTemplate
        repo
        (Just commit)
        filename
        $ githubGetFile auth

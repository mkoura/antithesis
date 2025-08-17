{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Validation.DownloadFile
    ( DownloadedFileFailure (..)
    , inspectDownloadedFileTemplate
    , inspectDownloadedFile
    , renderDownloadedFileFailure
    , analyzeDownloadedFile
    ) where

import Core.Types.Basic (Commit, Repository, FileName (..))
import Data.Text (Text)
import GitHub (Auth)
import Lib.GitHub (GetGithubFileFailure, githubGetFile)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import qualified Text.MMark as MMark

data DownloadedFileFailure
    = GithubGetFileError GetGithubFileFailure
    | DownloadedFileParseError String
    | DownloadedFileNotSupported
    deriving (Eq, Show)

renderDownloadedFileFailure :: DownloadedFileFailure -> String
renderDownloadedFileFailure = \case
    GithubGetFileError failure ->
        "Error when interacting with github. Details: " <> show failure
    DownloadedFileParseError failure ->
        "The downloaded file seems to have pare error. Details: " <> show failure
    DownloadedFileNotSupported ->
        "Only `md` and `yaml` files are currently supported in validation"

analyzeDownloadedFile
    :: FileName
    -> Either GetGithubFileFailure Text
    -> Either DownloadedFileFailure Text
analyzeDownloadedFile (FileName filename) = \case
    Left failure ->
        Left $ GithubGetFileError failure
    Right file ->
        if T.isSuffixOf "md" (T.pack filename) then
            case MMark.parse filename file of
                Left bundle ->
                    Left $ DownloadedFileParseError $ show bundle
                Right _ ->
                    Right file
        else if T.isSuffixOf "yaml" (T.pack filename) then
            -- not interested in value only if there is error hence chosen random type that happens to have FromJSON instance
            case Yaml.decodeEither' @Text (T.encodeUtf8 file) of
                Left parseError ->
                    Left $ DownloadedFileParseError $ show parseError
                Right _ ->
                    Right file
        else
            Left DownloadedFileNotSupported

inspectDownloadedFileTemplate
    :: Repository
    -> Maybe Commit
    -> FileName
    -> (Repository -> Maybe Commit -> FileName -> IO (Either GetGithubFileFailure Text))
    -> IO (Either DownloadedFileFailure Text)
inspectDownloadedFileTemplate repo commit filename downloadFile = do
    resp <- downloadFile repo commit filename
    pure $ analyzeDownloadedFile filename resp

inspectDownloadedFile
    :: Auth
    -> Repository
    -> Commit
    -> FileName
    -> IO (Either DownloadedFileFailure Text)
inspectDownloadedFile auth repo commit filename =
    inspectDownloadedFileTemplate
        repo
        (Just commit)
        filename
        $ githubGetFile auth

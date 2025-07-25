{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Validation.RegisterRole
    ( RepositoryRoleFailure (..)
    , inspectRepoRoleForUserTemplate
    , inspectRepoRoleForUser
    , renderRepositoryRoleFailure
    ) where

import Core.Types.Basic (Repository, Username (..))
import Data.List qualified as L
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Lib.GitHub (GetCodeOwnersFileFailure, githubGetCodeOwnersFile)

data RepositoryRoleFailure
    = NoRoleEntryInCodeowners
    | NoUsersAssignedToRoleInCodeowners
    | NoUserInCodeowners
    | GithubGetError GetCodeOwnersFileFailure
    deriving (Eq, Show)

renderRepositoryRoleFailure :: RepositoryRoleFailure -> String
renderRepositoryRoleFailure = \case
    NoRoleEntryInCodeowners -> "CODEOWNERS in the repository does not contain the role entry."
    NoUsersAssignedToRoleInCodeowners ->
        "CODEOWNERS in the repository does not contain any users assigned to the role."
    NoUserInCodeowners -> "CODEOWNERS in the repository does not contain the user."
    GithubGetError failure ->
        "Error when interacting with github. Details: " <> show failure

-- In order to verify the role of the userX CODEOWNERS file is downloaded with
-- the expectation there a line:
-- role: user1 user2 .. userX .. userN
analyzeResponseCodeownersFile
    :: Username
    -> Either GetCodeOwnersFileFailure Text
    -> Maybe RepositoryRoleFailure
analyzeResponseCodeownersFile (Username user) = \case
    Left failure ->
        Just $ GithubGetError failure
    Right file ->
        if null (lineWithRole file)
            then
                Just NoRoleEntryInCodeowners
            else
                if users file == [Nothing]
                    then
                        Just NoUsersAssignedToRoleInCodeowners
                    else
                        if foundUser file == [[]]
                            then
                                Just NoUserInCodeowners
                            else
                                Nothing
  where
    strBS = "antithesis"
    lineWithRole file = L.filter (T.isPrefixOf strBS) (T.lines file)
    colon = "antithesis" <> ": "
    getUsersWithRole = T.stripPrefix colon
    users file =
        getUsersWithRole
            <$> L.take 1 (lineWithRole file)
    foundUser file =
        L.filter (== ("@" <> T.pack user)) . T.words
            <$> catMaybes (users file)

inspectRepoRoleForUserTemplate
    :: Username
    -> Repository
    -> (Repository -> IO (Either GetCodeOwnersFileFailure Text))
    -> IO (Maybe RepositoryRoleFailure)
inspectRepoRoleForUserTemplate username repo downloadCodeownersFile = do
    resp <- downloadCodeownersFile repo
    pure $ analyzeResponseCodeownersFile username resp

inspectRepoRoleForUser
    :: Username
    -> Repository
    -> IO (Maybe RepositoryRoleFailure)
inspectRepoRoleForUser username repo =
    inspectRepoRoleForUserTemplate
        username
        repo
        githubGetCodeOwnersFile

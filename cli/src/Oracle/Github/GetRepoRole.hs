{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Oracle.Github.GetRepoRole
    ( RepoRoleValidation (..)
    , inspectRepoRoleForUserTemplate
    , inspectRepoRoleForUser
    , emitRepoRoleMsg
    ) where

import Data.Text (Text)
import Types (Repository, Role (..), Username)

import qualified Data.Text as T
import qualified Oracle.Github.CommonIO as IO
import qualified Oracle.Github.GetRepoRoleIO as IO

data RepoRoleValidation
    = RepoRoleValidated
    | NoRoleInRepo
    | NonexistantRolePicked
    | WrongRolePicked
    deriving (Eq, Show)

emitRepoRoleMsg :: RepoRoleValidation -> String
emitRepoRoleMsg = \case
    RepoRoleValidated -> "The user's role is validated in a given Github repository."
    NoRoleInRepo -> "User does not have any role in the repository."
    NonexistantRolePicked ->
        "Role picked does not exist. Please use one of "
            <> unlines (T.unpack <$> permissionDomain)
    WrongRolePicked ->
        "Role picked is not exactly the one in a repository user has attributed."

permissionDomain :: [Text]
permissionDomain = ["admin", "write", "read", "none"]

analyzeRepoRoleResponse
    :: Role -> IO.ResponseRepoRole -> RepoRoleValidation
analyzeRepoRoleResponse (Role roleToValidate) (IO.ResponseRepoRole roleResp)
    | roleResp == "none" = NoRoleInRepo
    | T.pack roleToValidate `notElem` permissionDomain =
        NonexistantRolePicked
    | T.pack roleToValidate /= roleResp = WrongRolePicked
    | otherwise = RepoRoleValidated

inspectRepoRoleForUserTemplate
    :: Username
    -> Repository
    -> Role
    -> IO IO.GithubAccessToken
    -> ( IO.GithubAccessToken
         -> Username
         -> Repository
         -> IO IO.ResponseRepoRole
       )
    -> IO RepoRoleValidation
inspectRepoRoleForUserTemplate username repo roleExpected getAccessToken requestRepoRoleForUser = do
    token <- getAccessToken
    resp <- requestRepoRoleForUser token username repo
    pure $ analyzeRepoRoleResponse roleExpected resp

inspectRepoRoleForUser
    :: Username
    -> Repository
    -> Role
    -> IO RepoRoleValidation
inspectRepoRoleForUser username repo roleExpected =
    inspectRepoRoleForUserTemplate
        username
        repo
        roleExpected
        IO.getGithubAccessToken
        IO.requestRepoRoleForUser

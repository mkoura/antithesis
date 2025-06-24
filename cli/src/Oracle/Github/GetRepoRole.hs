{-# LANGUAGE StrictData #-}

module Oracle.Github.GetRepoRole
    ( RepoRoleValidation (..)
    , inspectRepoRoleForUserTemplate
    , inspectRepoRoleForUser
    , emitRepoRoleMsg
    ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.List qualified as L
import Data.Maybe (catMaybes)
import Oracle.Github.GetRepoRoleIO qualified as IO
import Types (Repository, Role (..), Username (..))

data RepoRoleValidation
    = RepoRoleValidated
    | NoRoleEntryInCodeowners
    | NoUsersAssignedToRoleInCodeowners
    | NoUserInCodeowners
    deriving (Eq, Show)

emitRepoRoleMsg :: RepoRoleValidation -> String
emitRepoRoleMsg = \case
    RepoRoleValidated -> "The user's role is validated in a given Github repository."
    NoRoleEntryInCodeowners -> "CODEOWNERS in the repository does not contain the role entry."
    NoUsersAssignedToRoleInCodeowners ->
        "CODEOWNERS in the repository does not contain any users assigned to the role."
    NoUserInCodeowners -> "CODEOWNERS in the repository does not contain the user."

-- In order to verify the role of the userX CODEOWNERS file is downloaded with
-- the expectation there a line:
-- role: user1 user2 .. userX .. userN
analyzeResponseCodeownersFile
    :: Role -> Username -> IO.ResponseCodeownersFile -> RepoRoleValidation
analyzeResponseCodeownersFile (Role role) (Username user) (IO.ResponseCodeownersFile file)
    | null lineWithRole = NoRoleEntryInCodeowners
    | users == [Nothing] = NoUsersAssignedToRoleInCodeowners
    | foundUser == [[]] = NoUserInCodeowners
    | otherwise = RepoRoleValidated
  where
    linefeed = 10
    fileLines = BS.splitWith (== linefeed) $ BC.toStrict file
    strBS = BC.pack role
    lineWithRole = L.filter (BS.isPrefixOf strBS) fileLines
    colon = BC.pack $ role <> ": "
    getUsersWithRole = BS.stripPrefix colon
    users =
        getUsersWithRole
            <$> L.take 1 lineWithRole
    space = 32
    foundUser =
        L.filter (== (BC.pack $ "@" <> user)) . BS.split space
            <$> catMaybes users

inspectRepoRoleForUserTemplate
    :: Username
    -> Repository
    -> Role
    -> ( Username
         -> Repository
         -> IO IO.ResponseCodeownersFile
       )
    -> IO RepoRoleValidation
inspectRepoRoleForUserTemplate username repo roleExpected downloadCodeownersFile = do
    resp <- downloadCodeownersFile username repo
    pure $ analyzeResponseCodeownersFile roleExpected username resp

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
        IO.downloadCodeownersFile

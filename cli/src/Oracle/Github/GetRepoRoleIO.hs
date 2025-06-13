{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Oracle.Github.GetRepoRoleIO
    ( ResponseRepoRole (..)
    , requestRepoRoleForUser
    ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (FromJSON)
import Data.Aeson.Lens (_String, key)
import Data.Text (Text)
import GHC.Generics (Generic)
import Oracle.Github.CommonIO ( GithubAccessToken (..) )
import Types (Username (..), Repository (..))

import qualified Network.Wreq as Wreq

-- https://docs.github.com/en/rest/collaborators/collaborators?apiVersion=2022-11-28&versionId=free-pro-team%40latest&category=orgs&subcategory=organization-roles#get-repository-permissions-for-a-user
newtype ResponseRepoRole = ResponseRepoRole
    { role :: Text
    }
    deriving (Eq, Generic, Show)

instance FromJSON ResponseRepoRole

requestRepoRoleForUser
    :: GithubAccessToken
    -> Username
    -> Repository
    -> IO ResponseRepoRole
requestRepoRoleForUser token (Username username) repo = do
    response <- Wreq.getWith headers endpoint
    case response ^. Wreq.responseStatus . Wreq.statusCode of
        200 -> pure $ ResponseRepoRole (response ^. Wreq.responseBody . key "permission" . _String)
        _ -> error $ show $ response ^. Wreq.responseStatus
  where
    headers =
        Wreq.defaults
            & Wreq.header "Accept" .~ ["application/vnd.github+json"]
            & Wreq.header "Authorization" .~ ["Bearer" <> tokenPayload token]
            & Wreq.header "X-GitHub-Api-Version" .~ ["2022-11-28"]
    endpoint =
        "https://api.github.com/repos/"
        <> organization repo
        <> "/"
        <> project repo
        <> "/collaborators/"
        <> username
        <> "/permission"

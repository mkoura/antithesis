{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Oracle.Github.ListPublicKeysIO
    ( ResponsePublicKey (..)
    , requestListingOfPublicKeysForUser
    ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Oracle.Github.CommonIO (GithubAccessToken (..))
import Types (Username (..))

import qualified Network.Wreq as Wreq

-- https://docs.github.com/en/rest/users/keys?apiVersion=2022-11-28#list-public-keys-for-a-user
data ResponsePublicKey = ResponsePublicKey
    { id :: Int
    , key :: Text
    }
    deriving (Eq, Generic, Show)

instance FromJSON ResponsePublicKey

requestListingOfPublicKeysForUser
    :: GithubAccessToken
    -> Username
    -> IO [ResponsePublicKey]
requestListingOfPublicKeysForUser token (Username username) = do
    response <- Wreq.getWith headers endpoint
    case response ^. Wreq.responseStatus . Wreq.statusCode of
        200 -> (^. Wreq.responseBody) <$> Wreq.asJSON response
        _ -> error $ show $ response ^. Wreq.responseStatus
  where
    headers =
        Wreq.defaults
            & Wreq.header "Accept" .~ ["application/vnd.github+json"]
            & Wreq.header "Authorization" .~ ["Bearer" <> tokenPayload token]
            & Wreq.header "X-GitHub-Api-Version" .~ ["2022-11-28"]
    endpoint =
        "https://api.github.com/users/"
            <> username
            <> "/keys"

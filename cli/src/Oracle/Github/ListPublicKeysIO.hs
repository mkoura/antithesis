{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Oracle.Github.ListPublicKeysIO
    ( GithubAccessToken (..)
    , ResponsePublicKey (..)
    , getGithubAccessToken
    , requestListingOfPublicKeysForUser
    ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Types (Username (..))

import qualified Data.ByteString.Char8 as BC
import qualified Network.Wreq as Wreq

newtype GithubAccessToken = GithubAccessToken
    { tokenPayload :: ByteString
    }
    deriving (Eq, Show)

getGithubAccessToken :: IO GithubAccessToken
getGithubAccessToken = do
    GithubAccessToken . BC.pack . fromMaybe (error errMsg)
        <$> lookupEnv accessToken
  where
    accessToken = "GITHUB_PERSONAL_ACCESS_TOKEN"
    errMsg =
        unlines
            [ accessToken <> " is not set."
            , ""
            , "See how to generate the token :"
            , "https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens#creating-a-fine-grained-personal-access-token"
            ]

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

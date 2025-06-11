{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Oracle.Github.FactRequester
    ( GithubAccessToken (..)
    --, inspectPublicKey
    ) where

import Data.Aeson
    ( FromJSON (parseJSON)
    , KeyValue ((.=))
    , ToJSON (toJSON)
    , object
    , withObject
    , (.:)
    )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Environment
    ( lookupEnv )
import Types (PublicKeyHash)

newtype GithubAccessToken = GithubAccessToken
    { key :: String
    }
    deriving (Eq, Show)

getGithubAccessToken :: IO GithubAccessToken
getGithubAccessToken = do
    GithubAccessToken . fromMaybe (error errMsg) <$> lookupEnv accessToken
  where
    accessToken = "GITHUB_PERSONAL_ACCESS_TOKEN"
    errMsg = unlines
        [ accessToken <> " is not set."
        , ""
        , "See how to generate the token : https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens#creating-a-fine-grained-personal-access-token"
        ]

--inspectPublicKey :: UserName -> IO [PublicKeyHash]
--inspectPublicKey username = undefined

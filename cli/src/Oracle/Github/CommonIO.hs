{-# LANGUAGE StrictData #-}

module Oracle.Github.CommonIO
    ( GithubAccessToken (..)
    , getGithubAccessToken
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Oracle.Github.ListPublicKeys
    ( PublicKeyValidation (..)
    , inspectPublicKey
    ) where

import Types (Username, PublicKeyHash)

import qualified Data.Text as T
import qualified Oracle.Github.ListPublicKeysIO as IO

data PublicKeyValidation =
    PublicKeyValidated |
    NoPublicKeyFound |
    NoEd25519KeyFound
    deriving (Eq, Show)

analyzePublicKeyResponse ::[IO.ResponsePublicKey] -> PublicKeyValidation
analyzePublicKeyResponse = cond
  where
      cond resp
          | null resp = NoPublicKeyFound
          | not (any hasEpectedPrefix resp) = NoEd25519KeyFound
          | otherwise = PublicKeyValidated
      hasEpectedPrefix = T.isPrefixOf "ssh-ed25519" . IO.key

inspectPublicKey
    :: Username
    -> PublicKeyHash
    -> IO IO.GithubAccessToken
    -> (IO.GithubAccessToken -> Username -> IO [IO.ResponsePublicKey])
    -> IO PublicKeyValidation
inspectPublicKey username _pubKeyExpected getAccessToken requestPublicKeysForUser = do
    token <- getAccessToken
    resp <- requestPublicKeysForUser token username
    pure $ analyzePublicKeyResponse resp

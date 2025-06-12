{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Oracle.Github.ListPublicKeys
    ( PublicKeyValidation (..)
    , inspectPublicKey
    ) where

import Types (Username, PublicKeyHash)

import qualified Oracle.Github.ListPublicKeysIO as IO

data PublicKeyValidation =
    PublicKeyValidated | NoPublicKeyFound
    deriving (Eq, Show)

analyzePublicKeyResponse ::[IO.ResponsePublicKey] -> PublicKeyValidation
analyzePublicKeyResponse resp
    | null resp = NoPublicKeyFound
    | otherwise = PublicKeyValidated

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

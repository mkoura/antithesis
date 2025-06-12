{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Oracle.Github.ListPublicKeys
    ( PublicKeyValidation (..)
    , inspectPublicKey
    ) where

import Data.Maybe (catMaybes)
import Types (Username, PublicKeyHash (..) )

import qualified Data.Text as T
import qualified Oracle.Github.ListPublicKeysIO as IO

data PublicKeyValidation =
    PublicKeyValidated |
    NoPublicKeyFound |
    NoEd25519KeyFound |
    NoEd25519KeyMatch
    deriving (Eq, Show)

analyzePublicKeyResponse
    :: PublicKeyHash
    -> [IO.ResponsePublicKey]
    -> PublicKeyValidation
analyzePublicKeyResponse (PublicKeyHash pubkeyToValidate) = cond
  where
      cond resp
          | null resp = NoPublicKeyFound
          | not (any hasExpectedPrefix resp) = NoEd25519KeyFound
          | hasTheKey resp = NoEd25519KeyMatch
          | otherwise = PublicKeyValidated

      expectedPrefix = "ssh-ed25519"
      hasExpectedPrefix = T.isPrefixOf expectedPrefix . IO.key
      hasTheKey =
          any (== (T.pack pubkeyToValidate)) .
          catMaybes .
          map (T.stripPrefix expectedPrefix . IO.key)

inspectPublicKey
    :: Username
    -> PublicKeyHash
    -> IO IO.GithubAccessToken
    -> (IO.GithubAccessToken -> Username -> IO [IO.ResponsePublicKey])
    -> IO PublicKeyValidation
inspectPublicKey username pubKeyExpected getAccessToken requestPublicKeysForUser = do
    token <- getAccessToken
    resp <- requestPublicKeysForUser token username
    pure $ analyzePublicKeyResponse pubKeyExpected resp

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Oracle.Github.ListPublicKeys
    ( PublicKeyValidation (..)
    , inspectPublicKeyTemplate
    , inspectPublicKey
    , emitPublicKeyMsg
    ) where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Types (Username, PublicKeyHash (..) )

import qualified Data.Text as T
import qualified Oracle.Github.ListPublicKeysIO as IO

data PublicKeyValidation =
    PublicKeyValidated |
    NoPublicKeyFound |
    NoEd25519KeyFound |
    NoEd25519KeyMatch
    deriving (Eq, Show)

emitPublicKeyMsg :: PublicKeyValidation -> String
emitPublicKeyMsg = \case
    PublicKeyValidated -> "Public key of the user validated in Github."
    NoPublicKeyFound -> "The user does not have any public key exposed in Github."
    NoEd25519KeyFound -> "The user is expected to have public key with '"
        <> T.unpack expectedPrefix
        <> "' exposed."
    NoEd25519KeyMatch -> "The user does not have the specified Ed25519 public key exposed in Github."

expectedPrefix :: Text
expectedPrefix = "ssh-ed25519"

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

      hasExpectedPrefix = T.isPrefixOf expectedPrefix . IO.key
      hasTheKey =
          any (== (T.pack pubkeyToValidate)) .
          catMaybes .
          map (T.stripPrefix expectedPrefix . IO.key)

inspectPublicKeyTemplate
    :: Username
    -> PublicKeyHash
    -> IO IO.GithubAccessToken
    -> (IO.GithubAccessToken -> Username -> IO [IO.ResponsePublicKey])
    -> IO PublicKeyValidation
inspectPublicKeyTemplate username pubKeyExpected getAccessToken requestPublicKeysForUser = do
    token <- getAccessToken
    resp <- requestPublicKeysForUser token username
    pure $ analyzePublicKeyResponse pubKeyExpected resp

inspectPublicKey
    :: Username
    -> PublicKeyHash
    -> IO PublicKeyValidation
inspectPublicKey username pubKeyExpected  =
    inspectPublicKeyTemplate username pubKeyExpected IO.getGithubAccessToken IO.requestListingOfPublicKeysForUser

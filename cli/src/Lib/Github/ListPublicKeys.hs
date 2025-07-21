{-# LANGUAGE StrictData #-}

module Lib.Github.ListPublicKeys
    ( PublicKeyValidation (..)
    , inspectPublicKeyTemplate
    , inspectPublicKey
    , emitPublicKeyMsg
    ) where

import Core.Types.Basic (PublicKeyHash (..), Username)
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lib.GitHub (githubUserPublicKeys)

data PublicKeyValidation
    = PublicKeyValidated
    | NoPublicKeyFound
    | NoEd25519KeyFound
    | NoEd25519KeyMatch
    deriving (Eq, Show)

emitPublicKeyMsg :: PublicKeyValidation -> String
emitPublicKeyMsg = \case
    PublicKeyValidated -> "Public key of the user validated in Github."
    NoPublicKeyFound -> "The user does not have any public key exposed in Github."
    NoEd25519KeyFound ->
        "The user is expected to have public key with '"
            <> T.unpack expectedPrefix
            <> "' exposed. And none was found"
    NoEd25519KeyMatch ->
        "The user does not have the specified Ed25519 public key exposed in Github."

expectedPrefix :: Text
expectedPrefix = "ssh-ed25519 "

analyzePublicKeyResponse
    :: PublicKeyHash
    -> [Text]
    -> PublicKeyValidation
analyzePublicKeyResponse (PublicKeyHash pubkeyToValidate) = cond
  where
    cond resp
        | null resp = NoPublicKeyFound
        | not (any hasExpectedPrefix resp) = NoEd25519KeyFound
        | hasNotTheKey resp = NoEd25519KeyMatch
        | otherwise = PublicKeyValidated

    hasExpectedPrefix = T.isPrefixOf expectedPrefix
    hasNotTheKey =
        L.notElem (T.pack pubkeyToValidate)
            . mapMaybe (T.stripPrefix expectedPrefix)

inspectPublicKeyTemplate
    :: Username
    -> PublicKeyHash
    -> (Username -> IO [Text])
    -> IO PublicKeyValidation
inspectPublicKeyTemplate username pubKeyExpected requestPublicKeysForUser = do
    resp <- requestPublicKeysForUser username
    pure $ analyzePublicKeyResponse pubKeyExpected resp

inspectPublicKey
    :: Username
    -> PublicKeyHash
    -> IO PublicKeyValidation
inspectPublicKey username pubKeyExpected =
    inspectPublicKeyTemplate
        username
        pubKeyExpected
        githubUserPublicKeys

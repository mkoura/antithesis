{-# LANGUAGE StrictData #-}

module Validation.RegisterUser
    ( PublicKeyFailure (..)
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

data PublicKeyFailure
    = NoPublicKeyFound
    | NoEd25519KeyFound
    | NoEd25519KeyMatch
    deriving (Eq, Show)

emitPublicKeyMsg :: PublicKeyFailure -> String
emitPublicKeyMsg = \case
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
    -> Maybe PublicKeyFailure
analyzePublicKeyResponse (PublicKeyHash pubkeyToValidate) = cond
  where
    cond resp
        | null resp = Just NoPublicKeyFound
        | not (any hasExpectedPrefix resp) = Just NoEd25519KeyFound
        | hasNotTheKey resp = Just NoEd25519KeyMatch
        | otherwise = Nothing

    hasExpectedPrefix = T.isPrefixOf expectedPrefix
    hasNotTheKey =
        L.notElem (T.pack pubkeyToValidate)
            . mapMaybe (T.stripPrefix expectedPrefix)

inspectPublicKeyTemplate
    :: Username
    -> PublicKeyHash
    -> (Username -> IO [Text])
    -> IO (Maybe PublicKeyFailure)
inspectPublicKeyTemplate username pubKeyExpected requestPublicKeysForUser = do
    resp <- requestPublicKeysForUser username
    pure $ analyzePublicKeyResponse pubKeyExpected resp

inspectPublicKey
    :: Username
    -> PublicKeyHash
    -> IO (Maybe PublicKeyFailure)
inspectPublicKey username pubKeyExpected =
    inspectPublicKeyTemplate
        username
        pubKeyExpected
        githubUserPublicKeys

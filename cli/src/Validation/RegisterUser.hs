{-# LANGUAGE StrictData #-}

module Validation.RegisterUser
    ( PublicKeyFailure (..)
    , inspectPublicKeyTemplate
    , inspectPublicKey
    , renderPublicKeyFailure
    ) where

import Core.Types.Basic (PublicKeyHash (..), Username)
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lib.GitHub (GithubResponseError, githubUserPublicKeys)

data PublicKeyFailure
    = NoPublicKeyFound
    | NoEd25519KeyFound
    | NoEd25519KeyMatch
    | GithubError String
    deriving (Eq, Show)

renderPublicKeyFailure :: PublicKeyFailure -> String
renderPublicKeyFailure = \case
    NoPublicKeyFound -> "The user does not have any public key exposed in Github."
    NoEd25519KeyFound ->
        "The user is expected to have public key with '"
            <> T.unpack expectedPrefix
            <> "' exposed. And none was found"
    NoEd25519KeyMatch ->
        "The user does not have the specified Ed25519 public key exposed in Github."
    GithubError err ->
        "The following github error was encountered: " <> err

expectedPrefix :: Text
expectedPrefix = "ssh-ed25519 "

analyzePublicKeyResponse
    :: PublicKeyHash
    -> Either GithubResponseError [Text]
    -> Maybe PublicKeyFailure
analyzePublicKeyResponse (PublicKeyHash pubkeyToValidate) = \case
    Left err -> Just $ GithubError $ show err
    Right resp ->
        if null resp
            then
                Just NoPublicKeyFound
            else
                if not (any hasExpectedPrefix resp)
                    then
                        Just NoEd25519KeyFound
                    else
                        if hasNotTheKey resp
                            then
                                Just NoEd25519KeyMatch
                            else
                                Nothing
  where
    hasExpectedPrefix = T.isPrefixOf expectedPrefix
    hasNotTheKey =
        L.notElem (T.pack pubkeyToValidate)
            . mapMaybe (T.stripPrefix expectedPrefix)

inspectPublicKeyTemplate
    :: Username
    -> PublicKeyHash
    -> (Username -> IO (Either GithubResponseError [Text]))
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

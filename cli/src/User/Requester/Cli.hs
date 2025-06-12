{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module User.Requester.Cli
    ( requesterCmd
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode, object, (.=))
import Data.Binary.Builder (toLazyByteString)
import Data.Text (Text)
import Network.HTTP.Types (encodePathSegmentsRelative)
import Oracle.Github.ListPublicKeys
    ( PublicKeyValidation (..)
    , emitPublicKeyMsg
    , inspectPublicKey
    )
import Oracle.Token.API
    ( getTokenFacts
    , requestChange
    , retractRequest
    )
import Servant.Client (ClientM)
import Types
    ( Directory (..)
    , Operation (..)
    , OutputReference (..)
    , Platform (..)
    , PublicKeyHash (..)
    , Repository (..)
    , Request (..)
    , RequesterCommand (..)
    , Role (..)
    , SHA1 (..)
    , TokenId
    , Username (..)
    )

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

requesterCmd :: RequesterCommand -> ClientM Value
requesterCmd command = do
    case command of
        RegisterPublicKey{platform, username, pubkeyhash, tokenId} ->
            manageUser tokenId platform username pubkeyhash Insert
        UnregisterPublicKey{platform, username, pubkeyhash, tokenId} ->
            manageUser tokenId platform username pubkeyhash Delete
        RegisterRole{platform, repository, username, role, tokenId} ->
            manageRole tokenId platform repository username role Insert
        UnregisterRole{platform, repository, username, role, tokenId} ->
            manageRole tokenId platform repository username role Delete
        RequestTest
            { platform
            , repository
            , username
            , commit
            , directory
            , tokenId
            } ->
                requestTestCLI tokenId platform repository username commit directory
        RetractRequest
            (OutputReference{outputReferenceTx, outputReferenceIndex}) ->
                retractRequest outputReferenceTx outputReferenceIndex
        GetFacts{tokenId} -> getTokenFacts tokenId

mkKey :: [String] -> String
mkKey =
    BL.unpack
        . toLazyByteString
        . encodePathSegmentsRelative
        . fmap T.pack

requestTestCLI
    :: TokenId
    -> Platform
    -> Repository
    -> Username
    -> SHA1
    -> Directory
    -> ClientM Value
requestTestCLI
    tokenId
    (Platform platform)
    (Repository org repo)
    (Username username)
    (SHA1 sha1)
    (Directory directory) =
        requestChange tokenId
            $ Request
                { key =
                    mkKey
                        ["request-test-run", platform, org, repo, username, sha1, directory]
                , value =
                    BL.unpack
                        $ encode
                        $ object
                            [ "state" .= ("pending" :: Text)
                            ]
                , operation = Insert
                }

manageUser
    :: TokenId
    -> Platform
    -> Username
    -> PublicKeyHash
    -> Operation
    -> ClientM Value
manageUser
    tokenId
    (Platform platform)
    user@(Username username)
    pubkey@(PublicKeyHash pubkeyhash)
    operation = case operation of
        Insert -> do
            validation <- liftIO $ inspectPublicKey user pubkey
            case validation of
                PublicKeyValidated ->
                    makeMPFSChange
                notValidated -> error $ emitPublicKeyMsg notValidated
        _ ->
            makeMPFSChange
      where
        makeMPFSChange =
            requestChange tokenId
                $ Request
                    { key = mkKey ["register-public-key", platform, username, pubkeyhash]
                    , value = ""
                    , operation
                    }

manageRole
    :: TokenId
    -> Platform
    -> Repository
    -> Username
    -> Role
    -> Operation
    -> ClientM Value
manageRole
    tokenId
    (Platform platform)
    (Repository org repo)
    (Username username)
    (Role role)
    operation =
        requestChange tokenId
            $ Request
                { key = mkKey ["register-role", platform, org, repo, username, role]
                , value = ""
                , operation
                }

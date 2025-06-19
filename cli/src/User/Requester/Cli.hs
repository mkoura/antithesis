{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module User.Requester.Cli
    ( requesterCmd
    ) where

import Data.Aeson (ToJSON (..), Value (..), encode, object, (.=))
import Data.Binary.Builder (toLazyByteString)
import Data.Text (Text)
import Network.HTTP.Types (encodePathSegmentsRelative)
import Oracle.Token.API
    ( getTokenFacts
    , requestChange
    , retractChange
    )
import Servant.Client (ClientM)
import Submitting (submitting)
import Types
    ( Directory (..)
    , Operation (..)
    , Platform (..)
    , PublicKeyHash (..)
    , Repository (..)
    , RequesterCommand (..)
    , Role (..)
    , SHA1 (..)
    , TokenId
    , Username (..)
    , Wallet (..)
    , WithTxHash
    )

import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

requesterCmd :: Wallet -> TokenId -> RequesterCommand -> ClientM Value
requesterCmd wallet tokenId command = do
    case command of
        RegisterPublicKey{platform, username, pubkeyhash} ->
            toJSON
                <$> manageUser wallet tokenId platform username pubkeyhash Insert
        UnregisterPublicKey{platform, username, pubkeyhash} ->
            toJSON
                <$> manageUser wallet tokenId platform username pubkeyhash Delete
        RegisterRole{platform, repository, username, role} ->
            toJSON
                <$> manageRole wallet tokenId platform repository username role Insert
        UnregisterRole{platform, repository, username, role} ->
            toJSON
                <$> manageRole wallet tokenId platform repository username role Delete
        RequestTest
            { platform
            , repository
            , username
            , commit
            , directory
            } ->
                toJSON
                    <$> requestTestCLI
                        wallet
                        tokenId
                        platform
                        repository
                        username
                        commit
                        directory
        RetractRequest refId -> fmap toJSON $ submitting wallet $ \address ->
            retractChange address refId
        GetFacts -> getTokenFacts tokenId

mkKey :: [String] -> String
mkKey =
    BL.unpack
        . toLazyByteString
        . encodePathSegmentsRelative
        . fmap T.pack

requestTestCLI
    :: Wallet
    -> TokenId
    -> Platform
    -> Repository
    -> Username
    -> SHA1
    -> Directory
    -> ClientM WithTxHash
requestTestCLI
    wallet
    tokenId
    (Platform platform)
    (Repository org repo)
    (Username username)
    (SHA1 sha1)
    (Directory directory) = do
        submitting wallet $ \address -> do
            let key =
                    mkKey
                        [ "request-test-run"
                        , platform
                        , org
                        , repo
                        , username
                        , sha1
                        , directory
                        ]
                value =
                    BL.unpack
                        $ encode
                        $ object
                            [ "state" .= ("pending" :: Text)
                            ]
            requestChange address tokenId key value Insert

manageUser
    :: Wallet
    -> TokenId
    -> Platform
    -> Username
    -> PublicKeyHash
    -> Operation
    -> ClientM WithTxHash
manageUser
    wallet
    tokenId
    (Platform platform)
    (Username username)
    (PublicKeyHash pubkeyhash)
    operation =
        submitting wallet $ \address -> do
            let
                key =
                    mkKey
                        [ "register-user"
                        , platform
                        , username
                        , pubkeyhash
                        ]
            r <- requestChange address tokenId key "" operation
            liftIO $ print r
            return r

-- validation <- liftIO $ inspectPublicKey user pubkey
-- case validation of
--     PublicKeyValidated ->
--         makeMPFSChange
--     notValidated -> error $ emitPublicKeyMsg notValidated

manageRole
    :: Wallet
    -> TokenId
    -> Platform
    -> Repository
    -> Username
    -> Role
    -> Operation
    -> ClientM WithTxHash
manageRole
    wallet
    tokenId
    (Platform platform)
    (Repository org repo)
    (Username username)
    (Role roleStr)
    operation =
        submitting wallet $ \address -> do
            let key =
                    mkKey
                        [ "register-role"
                        , platform
                        , org
                        , repo
                        , username
                        , roleStr
                        ]
            requestChange address tokenId key "" operation

-- validation <- liftIO $ inspectRepoRoleForUser user rep role
-- case validation of
--     RepoRoleValidated ->
--         makeMPFSChange
--     notValidated -> error $ emitRepoRoleMsg notValidated

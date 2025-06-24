module User.Requester.Cli
    ( requesterCmd
    , RequesterCommand (..)
    ) where

import Core.Types
    ( Directory (..)
    , Platform (..)
    , PublicKeyHash (..)
    , Repository (..)
    , Role (..)
    , SHA1 (..)
    , TokenId
    , Username (..)
    , Wallet (..)
    , WithUnsignedTx
    )
import Data.Aeson (ToJSON (..), Value (..), encode, object, (.=))
import Data.Binary.Builder (toLazyByteString)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import MPFS.API
    ( requestDelete
    , requestInsert
    )
import Network.HTTP.Types (encodePathSegmentsRelative)
import Servant.Client (ClientM)
import Submitting (submittingFake)

data Operation = Insert | Delete
    deriving (Eq, Show)
data RequesterCommand
    = RegisterPublicKey
        { platform :: Platform
        , username :: Username
        , pubkeyhash :: PublicKeyHash
        }
    | UnregisterPublicKey
        { platform :: Platform
        , username :: Username
        , pubkeyhash :: PublicKeyHash
        }
    | RegisterRole
        { platform :: Platform
        , repository :: Repository
        , role :: Role
        , username :: Username
        }
    | UnregisterRole
        { platform :: Platform
        , repository :: Repository
        , role :: Role
        , username :: Username
        }
    | RequestTest
        { platform :: Platform
        , repository :: Repository
        , commit :: SHA1
        , directory :: Directory
        , username :: Username
        }
    deriving (Eq, Show)

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
                    <$> requestTestRun
                        wallet
                        tokenId
                        platform
                        repository
                        username
                        commit
                        directory

mkKey :: [String] -> String
mkKey =
    BL.unpack
        . toLazyByteString
        . encodePathSegmentsRelative
        . fmap T.pack

requestTestRun
    :: Wallet
    -> TokenId
    -> Platform
    -> Repository
    -> Username
    -> SHA1
    -> Directory
    -> ClientM WithUnsignedTx -- WithTxHash
requestTestRun
    wallet
    tokenId
    (Platform platform)
    (Repository org repo)
    (Username username)
    (SHA1 sha1)
    (Directory directory) = do
        submittingFake wallet $ \address -> do
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
            requestInsert address tokenId key value

manageUser
    :: Wallet
    -> TokenId
    -> Platform
    -> Username
    -> PublicKeyHash
    -> Operation
    -> ClientM WithUnsignedTx -- WithTxHash
manageUser
    wallet
    tokenId
    (Platform platform)
    (Username username)
    (PublicKeyHash pubkeyhash)
    operation =
        submittingFake wallet $ \address -> do
            let
                key =
                    mkKey
                        [ "register-user"
                        , platform
                        , username
                        , pubkeyhash
                        ]
                value = mempty
            case operation of
                Insert -> requestInsert address tokenId key value
                Delete -> requestDelete address tokenId key value

manageRole
    :: Wallet
    -> TokenId
    -> Platform
    -> Repository
    -> Username
    -> Role
    -> Operation
    -> ClientM WithUnsignedTx -- WithTxHash
manageRole
    wallet
    tokenId
    (Platform platform)
    (Repository org repo)
    (Username username)
    (Role roleStr)
    operation =
        submittingFake wallet $ \address -> do
            let key =
                    mkKey
                        [ "register-role"
                        , platform
                        , org
                        , repo
                        , username
                        , roleStr
                        ]
                value = mempty
            case operation of
                Insert -> requestInsert address tokenId key value
                Delete -> requestDelete address tokenId key value

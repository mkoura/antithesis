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
import Data.Binary.Builder (toLazyByteString)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Lib.JSON (object, (.=))
import MPFS.API
    ( RequestDeleteBody (..)
    , RequestInsertBody (..)
    , requestDelete
    , requestInsert
    )
import Network.HTTP.Types (encodePathSegmentsRelative)
import Servant.Client (ClientM)
import Submitting (submittingFake)
import Text.JSON.Canonical (JSValue (..), ToJSON (..), toJSString)
import User.Types (RegisterPublicKey (..), UnregisterPublicKey (..))

data Operation = Insert | Delete
    deriving (Eq, Show)

data RequesterCommand
    = RegisterUser RegisterPublicKey
    | UnregisterUser UnregisterPublicKey
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

requesterCmd
    :: Wallet -> TokenId -> RequesterCommand -> ClientM JSValue
requesterCmd wallet tokenId command = do
    case command of
        RegisterUser RegisterPublicKey{platform, username, pubkeyhash} ->
            manageUser wallet tokenId platform username pubkeyhash Insert
                >>= toJSON
        UnregisterUser UnregisterPublicKey{platform, username, pubkeyhash} ->
            manageUser wallet tokenId platform username pubkeyhash Delete
                >>= toJSON
        RegisterRole{platform, repository, username, role} ->
            manageRole wallet tokenId platform repository username role Insert
                >>= toJSON
        UnregisterRole{platform, repository, username, role} ->
            manageRole wallet tokenId platform repository username role Delete
                >>= toJSON
        RequestTest
            { platform
            , repository
            , username
            , commit
            , directory
            } ->
                requestTestRun
                    wallet
                    tokenId
                    platform
                    repository
                    username
                    commit
                    directory
                    >>= toJSON

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
    -> ClientM (WithUnsignedTx JSValue) -- WithTxHash
requestTestRun
    wallet
    tokenId
    (Platform platform)
    (Repository org repo)
    (Username username)
    (SHA1 sha1)
    (Directory directory) = do
        valueV <-
            object
                [ "state" .= ("pending" :: Text)
                ]
        submittingFake wallet $ \address -> do
            let key =
                    JSString
                        $ toJSString
                        $ mkKey
                            [ "request-test-run"
                            , platform
                            , org
                            , repo
                            , username
                            , sha1
                            , directory
                            ]

            requestInsert address tokenId
                $ RequestInsertBody key valueV

manageUser
    :: Wallet
    -> TokenId
    -> Platform
    -> Username
    -> PublicKeyHash
    -> Operation
    -> ClientM (WithUnsignedTx JSValue) -- WithTxHash
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
                    JSString
                        $ toJSString
                        $ mkKey
                            [ "register-user"
                            , platform
                            , username
                            , pubkeyhash
                            ]
                value = JSNull
            case operation of
                Insert -> requestInsert address tokenId $ RequestInsertBody key value
                Delete -> requestDelete address tokenId $ RequestDeleteBody key value

manageRole
    :: Wallet
    -> TokenId
    -> Platform
    -> Repository
    -> Username
    -> Role
    -> Operation
    -> ClientM (WithUnsignedTx JSValue) -- WithTxHash
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
                    JSString
                        $ toJSString
                        $ mkKey
                            [ "register-role"
                            , platform
                            , org
                            , repo
                            , username
                            , roleStr
                            ]
                value = JSNull
            case operation of
                Insert -> requestInsert address tokenId $ RequestInsertBody key value
                Delete -> requestDelete address tokenId $ RequestDeleteBody key value

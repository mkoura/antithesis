{-# LANGUAGE DuplicateRecordFields #-}

module User.Requester.Cli
    ( requesterCmd
    , RequesterCommand (..)
    ) where

import Core.Types
    ( TokenId
    , Wallet (..)
    , WithTxHash
    )
import MPFS.API
    ( RequestDeleteBody (..)
    , RequestInsertBody (..)
    , requestDelete
    , requestInsert
    )
import Servant.Client (ClientM)
import Submitting (submitting)
import Text.JSON.Canonical (JSValue (..), ToJSON (..))
import User.Agent.Cli
    ( agentCmd
    , create
    )
import User.Types
    ( Duration
    , RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    )

data RequesterCommand
    = RegisterUser RegisterUserKey
    | UnregisterUser RegisterUserKey
    | RegisterRole RegisterRoleKey
    | UnregisterRole RegisterRoleKey
    | RequestTest TestRun Duration
    deriving (Eq, Show)

requesterCmd
    :: Wallet -> TokenId -> RequesterCommand -> ClientM JSValue
requesterCmd wallet tokenId command = do
    case command of
        RegisterUser request ->
            registerUser wallet tokenId request >>= toJSON
        UnregisterUser request ->
            unregisterUser wallet tokenId request >>= toJSON
        RegisterRole request ->
            registerRole wallet tokenId request >>= toJSON
        UnregisterRole request ->
            unregisterRole wallet tokenId request >>= toJSON
        RequestTest testRun duration ->
            agentCmd wallet tokenId $ create testRun duration

registerUser
    :: Wallet
    -> TokenId
    -> RegisterUserKey
    -> ClientM (WithTxHash JSValue)
registerUser
    wallet
    tokenId
    request =
        submitting wallet $ \address -> do
            key <- toJSON request
            value <- toJSON ("" :: String)
            requestInsert address tokenId
                $ RequestInsertBody{key = key, value = value}

unregisterUser
    :: Wallet
    -> TokenId
    -> RegisterUserKey
    -> ClientM (WithTxHash JSValue)
unregisterUser
    wallet
    tokenId
    request =
        submitting wallet $ \address -> do
            key <- toJSON request
            value <- toJSON ("" :: String)
            requestDelete address tokenId
                $ RequestDeleteBody{key = key, value = value}

registerRole
    :: Wallet
    -> TokenId
    -> RegisterRoleKey
    -> ClientM (WithTxHash JSValue)
registerRole
    wallet
    tokenId
    request =
        submitting wallet $ \address -> do
            key <- toJSON request
            value <- toJSON ("" :: String)
            requestInsert address tokenId
                $ RequestInsertBody{key = key, value = value}

unregisterRole
    :: Wallet
    -> TokenId
    -> RegisterRoleKey
    -> ClientM (WithTxHash JSValue)
unregisterRole
    wallet
    tokenId
    request =
        submitting wallet $ \address -> do
            key <- toJSON request
            value <- toJSON ("" :: String)
            requestDelete address tokenId
                $ RequestDeleteBody{key = key, value = value}

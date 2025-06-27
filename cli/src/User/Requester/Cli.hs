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
    ( AgentCommand (..)
    , AgentCommandCore (..)
    , agentCmd
    )
import User.Types
    ( Direction (..)
    , Duration
    , RegisterPublicKey (..)
    , RegisterRoleKey (..)
    , TestRun (..)
    )

data RequesterCommand
    = RegisterUser RegisterPublicKey
    | RegisterRole RegisterRoleKey
    | RequestTest TestRun Duration
    deriving (Eq, Show)

requesterCmd
    :: Wallet -> TokenId -> RequesterCommand -> ClientM JSValue
requesterCmd wallet tokenId command = do
    case command of
        RegisterUser request ->
            manageUser wallet tokenId request >>= toJSON
        RegisterRole request ->
            manageRole wallet tokenId request >>= toJSON
        RequestTest testRun duration ->
            agentCmd wallet tokenId (AgentCommand $ Create testRun duration)

manageUser
    :: Wallet
    -> TokenId
    -> RegisterPublicKey
    -> ClientM WithTxHash
manageUser
    wallet
    tokenId
    request@RegisterPublicKey{direction} =
        submitting wallet $ \address -> do
            key <- toJSON request
            value <- toJSON ("" :: String)
            case direction of
                Insert ->
                    requestInsert address tokenId
                        $ RequestInsertBody{key = key, value = value}
                Delete ->
                    requestDelete address tokenId
                        $ RequestDeleteBody{key = key, value = value}

manageRole
    :: Wallet
    -> TokenId
    -> RegisterRoleKey
    -> ClientM WithTxHash
manageRole
    wallet
    tokenId
    request@RegisterRoleKey{direction} =
        submitting wallet $ \address -> do
            key <- toJSON request
            value <- toJSON ("" :: String)
            case direction of
                Insert ->
                    requestInsert address tokenId
                        $ RequestInsertBody{key = key, value = value}
                Delete ->
                    requestDelete address tokenId
                        $ RequestDeleteBody{key = key, value = value}

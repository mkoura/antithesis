{-# LANGUAGE DuplicateRecordFields #-}

module User.Requester.Cli
    ( requesterCmd
    , RequesterCommand (..)
    ) where

import Core.Types
    ( TokenId
    , TxHash
    , Wallet (..)
    , WithTxHash (..)
    )
import MPFS.API
    ( RequestDeleteBody (..)
    , RequestInsertBody (..)
    , requestDelete
    , requestInsert
    )
import Servant.Client (ClientM)
import Submitting (submitting)
import Text.JSON.Canonical (ToJSON (..))
import User.Agent.Cli
    ( AgentCommand (..)
    , agentCmd
    )
import User.Types
    ( Duration
    , Phase (PendingT)
    , RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunState
    )

data RequesterCommand a where
    RegisterUser :: RegisterUserKey -> RequesterCommand TxHash
    UnregisterUser
        :: RegisterUserKey -> RequesterCommand TxHash
    RegisterRole
        :: RegisterRoleKey -> RequesterCommand TxHash
    UnregisterRole
        :: RegisterRoleKey -> RequesterCommand TxHash
    RequestTest
        :: TestRun
        -> Duration
        -> RequesterCommand (WithTxHash (TestRunState PendingT))

deriving instance Show (RequesterCommand a)
deriving instance Eq (RequesterCommand a)

requesterCmd
    :: Wallet -> TokenId -> RequesterCommand a -> ClientM a
requesterCmd wallet tokenId command = do
    case command of
        RegisterUser request ->
            registerUser wallet tokenId request
        UnregisterUser request ->
            unregisterUser wallet tokenId request
        RegisterRole request ->
            registerRole wallet tokenId request
        UnregisterRole request ->
            unregisterRole wallet tokenId request
        RequestTest testRun duration ->
            agentCmd wallet tokenId $ Create testRun duration

registerUser
    :: Wallet
    -> TokenId
    -> RegisterUserKey
    -> ClientM TxHash
registerUser
    wallet
    tokenId
    request = fmap txHash
        $ submitting wallet
        $ \address -> do
            key <- toJSON request
            value <- toJSON ()
            requestInsert address tokenId
                $ RequestInsertBody{key = key, value = value}

unregisterUser
    :: Wallet
    -> TokenId
    -> RegisterUserKey
    -> ClientM TxHash
unregisterUser
    wallet
    tokenId
    request = fmap txHash
        $ submitting wallet
        $ \address -> do
            key <- toJSON request
            value <- toJSON ()
            requestDelete address tokenId
                $ RequestDeleteBody{key = key, value = value}

registerRole
    :: Wallet
    -> TokenId
    -> RegisterRoleKey
    -> ClientM TxHash
registerRole
    wallet
    tokenId
    request = fmap txHash
        $ submitting wallet
        $ \address -> do
            key <- toJSON request
            value <- toJSON ()
            requestInsert address tokenId
                $ RequestInsertBody{key = key, value = value}

unregisterRole
    :: Wallet
    -> TokenId
    -> RegisterRoleKey
    -> ClientM TxHash
unregisterRole
    wallet
    tokenId
    request = fmap txHash
        $ submitting wallet
        $ \address -> do
            key <- toJSON request
            value <- toJSON ()
            requestDelete address tokenId
                $ RequestDeleteBody{key = key, value = value}

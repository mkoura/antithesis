{-# LANGUAGE DuplicateRecordFields #-}

module User.Requester.Cli
    ( requesterCmd
    , RequesterCommand (..)
    ) where

import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic (Duration, TokenId)
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Operation (Operation (..))
import Core.Types.Tx (TxHash, WithTxHash (..))
import Core.Types.Wallet (Wallet (..))
import Data.ByteString.Lazy qualified as BL
import Data.Functor (($>))
import Lib.SSH.Private (Sign)
import MPFS.API
    ( RequestDeleteBody (..)
    , RequestInsertBody (..)
    , requestDelete
    , requestInsert
    )
import Oracle.Validate.Requests.RegisterRole
    ( RegisterRoleFailure
    , UnregisterRoleFailure
    , validateRegisterRole
    , validateUnregisterRole
    )
import Oracle.Validate.Requests.RegisterUser
    ( RegisterUserFailure
    , UnregisterUserFailure
    , validateRegisterUser
    , validateUnregisterUser
    )
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig
    )
import Oracle.Validate.Requests.TestRun.Create
    ( CreateTestRunFailure
    , validateCreateTestRun
    )
import Oracle.Validate.Types
    ( AValidationResult
    , runValidate
    )
import Servant.Client (ClientM)
import Submitting (Submitting, signAndSubmit)
import Text.JSON.Canonical (ToJSON (..), renderCanonicalJSON)
import User.Types
    ( Phase (PendingT)
    , RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunState (..)
    )
import Validation (mkValidation)

data RequesterCommand a where
    RegisterUser
        :: RegisterUserKey
        -> RequesterCommand (AValidationResult RegisterUserFailure TxHash)
    UnregisterUser
        :: RegisterUserKey
        -> RequesterCommand (AValidationResult UnregisterUserFailure TxHash)
    RegisterRole
        :: RegisterRoleKey
        -> RequesterCommand (AValidationResult RegisterRoleFailure TxHash)
    UnregisterRole
        :: RegisterRoleKey
        -> RequesterCommand (AValidationResult UnregisterRoleFailure TxHash)
    RequestTest
        :: TestRun
        -> Duration
        -> RequesterCommand
            ( AValidationResult
                CreateTestRunFailure
                (WithTxHash (TestRunState PendingT))
            )

deriving instance Show (RequesterCommand a)
deriving instance Eq (RequesterCommand a)

requesterCmd
    :: Submitting
    -> Wallet
    -> TestRunValidationConfig
    -> TokenId
    -> Sign
    -> RequesterCommand a
    -> ClientM a
requesterCmd sbmt wallet testRunConfig tokenId sign command = do
    case command of
        RegisterUser request ->
            registerUser sbmt wallet tokenId request
        UnregisterUser request ->
            unregisterUser sbmt wallet tokenId request
        RegisterRole request ->
            registerRole sbmt wallet tokenId request
        UnregisterRole request ->
            unregisterRole sbmt wallet tokenId request
        RequestTest testRun duration ->
            createCommand
                sbmt
                wallet
                testRunConfig
                tokenId
                sign
                testRun
                duration

createCommand
    :: Submitting
    -> Wallet
    -> TestRunValidationConfig
    -> TokenId
    -> Sign
    -> TestRun
    -> Duration
    -> ClientM
        ( AValidationResult
            CreateTestRunFailure
            (WithTxHash (TestRunState PendingT))
        )
createCommand sbmt wallet testRunConfig tokenId sign testRun duration =
    runValidate $ do
        key <- toJSON testRun
        let signature = sign $ BL.toStrict $ renderCanonicalJSON key
        let newState = Pending duration signature
        void
            $ validateCreateTestRun testRunConfig (mkValidation tokenId)
            $ Change (Key testRun) (Insert newState)
        value <- toJSON newState
        wtx <- lift $ signAndSubmit sbmt wallet $ \address -> do
            requestInsert address tokenId
                $ RequestInsertBody{key, value}
        pure $ wtx $> newState

registerUser
    :: Submitting
    -> Wallet
    -> TokenId
    -> RegisterUserKey
    -> ClientM (AValidationResult RegisterUserFailure TxHash)
registerUser
    sbmt
    wallet
    tokenId
    request = runValidate $ do
        void
            $ validateRegisterUser (mkValidation tokenId)
            $ Change (Key request) (Insert ())
        fmap txHash
            $ lift
            $ signAndSubmit sbmt wallet
            $ \address -> do
                key <- toJSON request
                value <- toJSON ()
                requestInsert address tokenId
                    $ RequestInsertBody{key = key, value = value}

unregisterUser
    :: Submitting
    -> Wallet
    -> TokenId
    -> RegisterUserKey
    -> ClientM (AValidationResult UnregisterUserFailure TxHash)
unregisterUser
    sbmt
    wallet
    tokenId
    request = runValidate $ do
        void
            $ validateUnregisterUser (mkValidation tokenId)
            $ Change (Key request) (Delete ())
        fmap txHash
            $ lift
            $ signAndSubmit sbmt wallet
            $ \address -> do
                key <- toJSON request
                value <- toJSON ()
                requestDelete address tokenId
                    $ RequestDeleteBody{key = key, value = value}

registerRole
    :: Submitting
    -> Wallet
    -> TokenId
    -> RegisterRoleKey
    -> ClientM (AValidationResult RegisterRoleFailure TxHash)
registerRole
    sbmt
    wallet
    tokenId
    request = runValidate $ do
        void
            $ validateRegisterRole (mkValidation tokenId)
            $ Change (Key request) (Insert ())
        fmap txHash
            $ lift
            $ signAndSubmit sbmt wallet
            $ \address -> do
                key <- toJSON request
                value <- toJSON ()
                requestInsert address tokenId
                    $ RequestInsertBody{key = key, value = value}

unregisterRole
    :: Submitting
    -> Wallet
    -> TokenId
    -> RegisterRoleKey
    -> ClientM (AValidationResult UnregisterRoleFailure TxHash)
unregisterRole
    sbmt
    wallet
    tokenId
    request = runValidate $ do
        void
            $ validateUnregisterRole (mkValidation tokenId)
            $ Change (Key request) (Delete ())
        fmap txHash
            $ lift
            $ signAndSubmit sbmt wallet
            $ \address -> do
                key <- toJSON request
                value <- toJSON ()
                requestDelete address tokenId
                    $ RequestDeleteBody{key = key, value = value}

{-# LANGUAGE DuplicateRecordFields #-}

module User.Requester.Cli
    ( requesterCmd
    , RequesterCommand (..)
    ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Core.Context
    ( WithContext
    , askConfig
    , askMpfs
    , askSubmit
    , askValidation
    )
import Core.Types.Basic (Duration, TokenId)
import Core.Types.Change (Change (..), Key (..), deleteKey, insertKey)
import Core.Types.Operation (Operation (..))
import Core.Types.Tx (TxHash, WithTxHash (..))
import Core.Types.Wallet (Wallet)
import Data.ByteString.Lazy qualified as BL
import Data.Functor (($>))
import Lib.SSH.Private
    ( KeyAPI (..)
    , SSHClient (..)
    , decodePrivateSSHFile
    )
import MPFS.API
    ( MPFS (..)
    , RequestDeleteBody (..)
    , RequestInsertBody (..)
    )
import Oracle.Config.Types (Config (..))
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
import Oracle.Validate.Requests.TestRun.Create
    ( CreateTestRunFailure (..)
    , validateCreateTestRun
    )
import Oracle.Validate.Types
    ( AValidationResult
    , liftMaybe
    , runValidate
    )
import Submitting (Submission (..))
import Text.JSON.Canonical (ToJSON (..), renderCanonicalJSON)
import User.Types
    ( Phase (PendingT)
    , RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunState (..)
    )

data RequesterCommand a where
    RegisterUser
        :: TokenId
        -> Wallet
        -> RegisterUserKey
        -> RequesterCommand (AValidationResult RegisterUserFailure TxHash)
    UnregisterUser
        :: TokenId
        -> Wallet
        -> RegisterUserKey
        -> RequesterCommand (AValidationResult UnregisterUserFailure TxHash)
    RegisterRole
        :: TokenId
        -> Wallet
        -> RegisterRoleKey
        -> RequesterCommand (AValidationResult RegisterRoleFailure TxHash)
    UnregisterRole
        :: TokenId
        -> Wallet
        -> RegisterRoleKey
        -> RequesterCommand (AValidationResult UnregisterRoleFailure TxHash)
    RequestTest
        :: TokenId
        -> Wallet
        -> SSHClient
        -> TestRun
        -> Duration
        -> RequesterCommand
            ( AValidationResult
                CreateTestRunFailure
                (WithTxHash (TestRunState PendingT))
            )

deriving instance Show (RequesterCommand a)
deriving instance Eq (RequesterCommand a)

requesterCmd
    :: (MonadIO m, MonadMask m)
    => RequesterCommand a
    -> WithContext m a
requesterCmd command = do
    case command of
        RegisterUser tokenId wallet request ->
            registerUser tokenId wallet request
        UnregisterUser tokenId wallet request ->
            unregisterUser tokenId wallet request
        RegisterRole tokenId wallet request ->
            registerRole tokenId wallet request
        UnregisterRole tokenId wallet request ->
            unregisterRole tokenId wallet request
        RequestTest tokenId wallet sshClient testRun duration ->
            createCommand
                tokenId
                wallet
                sshClient
                testRun
                duration

createCommand
    :: (MonadIO m, MonadMask m)
    => TokenId
    -> Wallet
    -> SSHClient
    -> TestRun
    -> Duration
    -> WithContext
        m
        ( AValidationResult
            CreateTestRunFailure
            (WithTxHash (TestRunState PendingT))
        )
createCommand
    tokenId
    wallet
    sshClient
    testRun
    duration = do
        mconfig <- askConfig tokenId
        validation <- askValidation $ Just tokenId
        Submission submit <- ($ wallet) <$> askSubmit
        mpfs <- askMpfs
        lift $ runValidate $ do
            Config{configTestRun} <-
                liftMaybe CreateTestConfigNotAvailable mconfig
            key <- toJSON testRun
            KeyAPI{sign} <-
                liftMaybe CreateTestRunInvalidSSHKey
                    =<< lift (liftIO $ decodePrivateSSHFile sshClient)
            let signature = sign $ BL.toStrict $ renderCanonicalJSON key
            let newState = Pending duration signature
            void
                $ validateCreateTestRun configTestRun validation
                $ Change (Key testRun) (Insert newState)
            value <- toJSON newState
            wtx <- lift $ submit $ \address -> do
                mpfsRequestInsert mpfs address tokenId
                    $ RequestInsertBody{key, value}
            pure $ wtx $> newState

registerUser
    :: Monad m
    => TokenId
    -> Wallet
    -> RegisterUserKey
    -> WithContext m (AValidationResult RegisterUserFailure TxHash)
registerUser
    tokenId
    wallet
    request = do
        mpfs <- askMpfs
        Submission submit <- ($ wallet) <$> askSubmit
        validation <- askValidation $ Just tokenId
        lift $ runValidate $ do
            void
                $ validateRegisterUser validation
                $ insertKey request
            fmap txHash
                $ lift
                $ submit
                $ \address -> do
                    key <- toJSON request
                    value <- toJSON ()
                    mpfsRequestInsert mpfs address tokenId
                        $ RequestInsertBody{key = key, value = value}

unregisterUser
    :: Monad m
    => TokenId
    -> Wallet
    -> RegisterUserKey
    -> WithContext m (AValidationResult UnregisterUserFailure TxHash)
unregisterUser
    tokenId
    wallet
    request = do
        mpfs <- askMpfs
        validation <- askValidation $ Just tokenId
        Submission submit <- ($ wallet) <$> askSubmit
        lift $ runValidate $ do
            void
                $ validateUnregisterUser validation
                $ deleteKey request
            fmap txHash
                $ lift
                $ submit
                $ \address -> do
                    key <- toJSON request
                    value <- toJSON ()
                    mpfsRequestDelete mpfs address tokenId
                        $ RequestDeleteBody{key = key, value = value}

registerRole
    :: Monad m
    => TokenId
    -> Wallet
    -> RegisterRoleKey
    -> WithContext m (AValidationResult RegisterRoleFailure TxHash)
registerRole
    tokenId
    wallet
    request = do
        mpfs <- askMpfs
        validation <- askValidation $ Just tokenId
        Submission submit <- ($ wallet) <$> askSubmit
        lift $ runValidate $ do
            void
                $ validateRegisterRole validation
                $ insertKey request
            fmap txHash
                $ lift
                $ submit
                $ \address -> do
                    key <- toJSON request
                    value <- toJSON ()
                    mpfsRequestInsert mpfs address tokenId
                        $ RequestInsertBody{key = key, value = value}

unregisterRole
    :: Monad m
    => TokenId
    -> Wallet
    -> RegisterRoleKey
    -> WithContext m (AValidationResult UnregisterRoleFailure TxHash)
unregisterRole
    tokenId
    wallet
    request = do
        mpfs <- askMpfs
        validation <- askValidation $ Just tokenId
        Submission submit <- ($ wallet) <$> askSubmit
        lift $ runValidate $ do
            void
                $ validateUnregisterRole validation
                $ deleteKey request
            fmap txHash
                $ lift
                $ submit
                $ \address -> do
                    key <- toJSON request
                    value <- toJSON ()
                    mpfsRequestDelete mpfs address tokenId
                        $ RequestDeleteBody{key = key, value = value}

{-# LANGUAGE DuplicateRecordFields #-}

module User.Requester.Cli
    ( requesterCmd
    , RequesterCommand (..)
    ) where

import Control.Monad (void)
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
import Data.ByteString.Lazy qualified as BL
import Data.Functor (($>))
import Lib.SSH.Private (Sign)
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
        -> RegisterUserKey
        -> RequesterCommand (AValidationResult RegisterUserFailure TxHash)
    UnregisterUser
        :: TokenId
        -> RegisterUserKey
        -> RequesterCommand (AValidationResult UnregisterUserFailure TxHash)
    RegisterRole
        :: TokenId
        -> RegisterRoleKey
        -> RequesterCommand (AValidationResult RegisterRoleFailure TxHash)
    UnregisterRole
        :: TokenId
        -> RegisterRoleKey
        -> RequesterCommand (AValidationResult UnregisterRoleFailure TxHash)
    RequestTest
        :: TokenId
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
    :: MonadIO m
    => Sign
    -> RequesterCommand a
    -> WithContext m a
requesterCmd sign command = do
    case command of
        RegisterUser tokenId request ->
            registerUser tokenId request
        UnregisterUser tokenId request ->
            unregisterUser tokenId request
        RegisterRole tokenId request ->
            registerRole tokenId request
        UnregisterRole tokenId request ->
            unregisterRole tokenId request
        RequestTest tokenId testRun duration ->
            createCommand
                tokenId
                sign
                testRun
                duration

createCommand
    :: MonadIO m
    => TokenId
    -> Sign
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
    sign
    testRun
    duration = do
        mconfig <- askConfig tokenId
        validation <- askValidation tokenId
        Submission submit <- askSubmit
        mpfs <- askMpfs
        lift $ runValidate $ do
            Config{configTestRun} <-
                liftMaybe CreateTestConfigNotAvailable mconfig
            key <- toJSON testRun
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
    -> RegisterUserKey
    -> WithContext m (AValidationResult RegisterUserFailure TxHash)
registerUser
    tokenId
    request = do
        mpfs <- askMpfs
        Submission submit <- askSubmit
        validation <- askValidation tokenId
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
    -> RegisterUserKey
    -> WithContext m (AValidationResult UnregisterUserFailure TxHash)
unregisterUser
    tokenId
    request = do
        mpfs <- askMpfs
        validation <- askValidation tokenId
        Submission submit <- askSubmit
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
    -> RegisterRoleKey
    -> WithContext m (AValidationResult RegisterRoleFailure TxHash)
registerRole
    tokenId
    request = do
        mpfs <- askMpfs
        validation <- askValidation tokenId
        Submission submit <- askSubmit
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
    -> RegisterRoleKey
    -> WithContext m (AValidationResult UnregisterRoleFailure TxHash)
unregisterRole
    tokenId
    request = do
        mpfs <- askMpfs
        validation <- askValidation tokenId
        Submission submit <- askSubmit
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

{-# LANGUAGE DuplicateRecordFields #-}

module User.Requester.Cli
    ( requesterCmd
    , RequesterCommand (..)
    ) where

import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Core.Context
    ( WithContext
    , askMpfs
    , askSubmit
    , askTestRunConfig
    , askValidation
    )
import Core.Types.Basic (Duration, TokenId)
import Core.Types.Change (Change (..), Key (..))
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
    ( CreateTestRunFailure
    , validateCreateTestRun
    )
import Oracle.Validate.Types
    ( AValidationResult
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
    :: Monad m
    => TokenId
    -> Sign
    -> RequesterCommand a
    -> WithContext m a
requesterCmd tokenId sign command = do
    case command of
        RegisterUser request ->
            registerUser tokenId request
        UnregisterUser request ->
            unregisterUser tokenId request
        RegisterRole request ->
            registerRole tokenId request
        UnregisterRole request ->
            unregisterRole tokenId request
        RequestTest testRun duration ->
            createCommand
                tokenId
                sign
                testRun
                duration

createCommand
    :: Monad m
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
        testRunConfig <- askTestRunConfig
        validation <- askValidation tokenId
        Submission submit <- askSubmit
        mpfs <- askMpfs
        lift $ runValidate $ do
            key <- toJSON testRun
            let signature = sign $ BL.toStrict $ renderCanonicalJSON key
            let newState = Pending duration signature
            void
                $ validateCreateTestRun testRunConfig validation
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
                $ Change (Key request) (Insert ())
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
                $ Change (Key request) (Delete ())
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
                $ Change (Key request) (Insert ())
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
                $ Change (Key request) (Delete ())
            fmap txHash
                $ lift
                $ submit
                $ \address -> do
                    key <- toJSON request
                    value <- toJSON ()
                    mpfsRequestDelete mpfs address tokenId
                        $ RequestDeleteBody{key = key, value = value}

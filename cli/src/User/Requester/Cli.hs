{-# LANGUAGE DuplicateRecordFields #-}

module User.Requester.Cli
    ( requesterCmd
    , RequesterCommand (..)
    ) where

import Control.Monad (forM, forM_, void)
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
import Core.Types.Basic
    ( Directory (..)
    , Duration
    , FileName (..)
    , Repository (..)
    , TokenId
    )
import Core.Types.Change (Change (..), Key (..), deleteKey, insertKey)
import Core.Types.Operation (Operation (..))
import Core.Types.Tx (TxHash, WithTxHash (..))
import Core.Types.Wallet (Wallet)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy qualified as BL
import Data.Functor (($>))
import Data.Text.IO qualified as T
import Lib.JSON.Canonical.Extra (object, (.=))
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
    , ForRole (..)
    , liftMaybe
    , mapFailure
    , runValidate
    , sequenceValidate
    , throwLeft
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
import Validation (Validation (..))
import Validation.DownloadFile (DownloadedFileFailure)

newtype GenerateAssetsFailure
    = GenerateAssetsFailure [(FileName, DownloadedFileFailure)]

instance Monad m => ToJSON m GenerateAssetsFailure where
    toJSON (GenerateAssetsFailure failures) =
        object
            [
                ( "generateAssetsFailure"
                , mapM
                    (\(FileName fn, err) -> object ["file" .= fn, "error" .= err])
                    failures
                )
            ]

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
    GenerateAssets
        :: Directory
        -> RequesterCommand
            ( AValidationResult
                GenerateAssetsFailure
                ()
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
        GenerateAssets directory -> generateAssets directory

generateAssets
    :: MonadIO m
    => Directory
    -> WithContext m (AValidationResult GenerateAssetsFailure ())
generateAssets (Directory targetDirectory) = do
    Validation{githubGetFile} <- askValidation Nothing
    lift $ runValidate $ do
        downloads <- forM ["docker-compose.yaml", "README.md", "testnet.yaml"] $ \filename -> do
            fmap (bimap (FileName filename,) (filename,))
                $ lift
                $ githubGetFile
                    (Repository "cardano-foundation" "antithesis")
                    Nothing
                    (FileName $ "compose/testnets/cardano_node_master/" <> filename)
        contents <-
            mapFailure GenerateAssetsFailure
                $ sequenceValidate
                $ fmap (throwLeft id) downloads
        forM_ contents $ \(filename, content) -> do
            let filePath = targetDirectory <> "/" <> filename
            liftIO $ T.writeFile filePath content

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
        Submission submit <- askSubmit wallet
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
        Submission submit <- askSubmit wallet
        validation <- askValidation $ Just tokenId
        lift $ runValidate $ do
            void
                $ validateRegisterUser validation ForUser
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
        Submission submit <- askSubmit wallet
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
        Submission submit <- askSubmit wallet
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
        Submission submit <- askSubmit wallet
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

{-# LANGUAGE DeriveFunctor #-}

module Core.Context
    ( WithContext
    , Context
    , withContext
    , askMpfs
    , askTestRunConfig
    , askWalletOwner
    , askValidation
    , askSubmit
    , withMPFS
    ) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Core.Types.Basic (Owner, TokenId)
import MPFS.API (MPFS)
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig
    )
import Submitting (Submission)
import Validation (Validation)

-- it would be nice to modulate it a bit depending con the command but we do not
-- have a type for each command
data Context m = Context
    { ctxMPFS :: MPFS m
    , ctxTestRunConfig :: TestRunValidationConfig
    , ctxWalletOwner :: Owner
    , ctxMkValidation :: TokenId -> Validation m
    , ctxSubmit :: Submission m
    }

newtype WithContext m a = WithContext
    { _getWithContext :: ReaderT (Context m) m a
    }
    deriving (Functor, Applicative, Monad)

instance MonadTrans WithContext where
    lift = WithContext . lift

askMpfs :: Monad m => WithContext m (MPFS m)
askMpfs = ctxMPFS <$> WithContext ask

withMPFS :: Monad m => (MPFS m -> m a) -> WithContext m a
withMPFS f = do
    mpfs <- askMpfs
    lift $ f mpfs

askTestRunConfig :: Monad m => WithContext m TestRunValidationConfig
askTestRunConfig = ctxTestRunConfig <$> WithContext ask

askWalletOwner :: Monad m => WithContext m Owner
askWalletOwner = ctxWalletOwner <$> WithContext ask

askValidation :: Monad m => TokenId -> WithContext m (Validation m)
askValidation tokenId = do
    ctx <- WithContext ask
    return $ ctxMkValidation ctx tokenId

askSubmit :: Monad m => WithContext m (Submission m)
askSubmit = ctxSubmit <$> WithContext ask

withContext
    :: MPFS m
    -> TestRunValidationConfig
    -> Owner
    -> (TokenId -> Validation m)
    -> Submission m
    -> WithContext m a
    -> m a
withContext mpfs testRunConfig walletOwner mkValidation submit (WithContext action) =
    runReaderT
        action
        (Context mpfs testRunConfig walletOwner mkValidation submit)

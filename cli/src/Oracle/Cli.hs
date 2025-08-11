module Oracle.Cli
    ( OracleCommand (..)
    , oracleCmd
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Core.Context (WithContext)
import Core.Types.Basic (TokenId)
import Oracle.Token.Cli (TokenCommand, tokenCmdCore)
import Oracle.Validate.Cli (ValidateCommand, validateCmd)

data OracleCommand a where
    OracleTokenCommand :: TokenCommand a -> OracleCommand a
    OracleValidateCommand
        :: TokenId -> ValidateCommand a -> OracleCommand a

deriving instance Show (OracleCommand a)
deriving instance Eq (OracleCommand a)

oracleCmd
    :: MonadIO m
    => OracleCommand a
    -> WithContext m a
oracleCmd = \case
    OracleTokenCommand tokenCommand -> tokenCmdCore tokenCommand
    OracleValidateCommand tokenId validateCommand -> do
        validateCmd
            tokenId
            validateCommand

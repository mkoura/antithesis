module Oracle.Cli
    ( OracleCommand (..)
    , oracleCmd
    ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (..))
import Core.Context (WithContext)
import Oracle.Config.Cli (ConfigCmd (..), configCmd)
import Oracle.Token.Cli (TokenCommand, tokenCmdCore)

data OracleCommand a where
    OracleTokenCommand :: TokenCommand a -> OracleCommand a
    OracleSetConfigCommand
        :: ConfigCmd a -> OracleCommand a

deriving instance Show (OracleCommand a)
deriving instance Eq (OracleCommand a)

oracleCmd
    :: (MonadIO m, MonadMask m)
    => OracleCommand a
    -> WithContext m a
oracleCmd = \case
    OracleTokenCommand tokenCommand -> tokenCmdCore tokenCommand
    OracleSetConfigCommand configCommand -> configCmd configCommand

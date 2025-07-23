module Oracle.Cli
    ( OracleCommand (..)
    , oracleCmd
    ) where

import Core.Context (WithContext)
import Core.Types.Basic (TokenId)
import Oracle.Token.Cli (TokenCommand, tokenCmdCore)
import Oracle.Validate.Cli (ValidateCommand, validateCmd)

data OracleCommand a where
    OracleTokenCommand :: TokenCommand a -> OracleCommand a
    OracleValidateCommand :: ValidateCommand a -> OracleCommand a

deriving instance Show (OracleCommand a)
deriving instance Eq (OracleCommand a)

oracleCmd
    :: Monad m
    => Maybe TokenId
    -> OracleCommand a
    -> WithContext m a
oracleCmd mtk = \case
    OracleTokenCommand tokenCommand -> tokenCmdCore mtk tokenCommand
    OracleValidateCommand validateCommand -> do
        tk <- case mtk of
            Just tokenId -> pure tokenId
            Nothing -> error "TokenId is required for ValidateCommand"
        validateCmd
            tk
            validateCommand

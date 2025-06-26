module Oracle.Cli
    ( OracleCommand (..)
    , oracleCmd
    ) where

import Core.Types (TokenId, Wallet)
import Oracle.Token.Cli (TokenCommand, tokenCmd)
import Oracle.Validate.Cli (ValidateCommand, validateCmd)
import Servant.Client (ClientM)
import Text.JSON.Canonical (JSValue)

data OracleCommand
    = OracleTokenCommand TokenCommand
    | OracleValidateCommand ValidateCommand
    deriving (Eq, Show)

oracleCmd :: Wallet -> TokenId -> OracleCommand -> ClientM JSValue
oracleCmd wallet tk = \case
    OracleTokenCommand tokenCommand ->
        tokenCmd
            wallet
            tk
            tokenCommand
    OracleValidateCommand validateCommand ->
        validateCmd
            tk
            validateCommand

module Oracle.Cli
    ( OracleCommand (..)
    , oracleCmd
    ) where

import Core.Types (TokenId, Wallet)
import Oracle.Token.Cli (TokenCommand, tokenCmd)
import Servant.Client (ClientM)
import Text.JSON.Canonical (JSValue)

newtype OracleCommand
    = OracleTokenCommand TokenCommand
    deriving (Eq, Show)

oracleCmd :: Wallet -> TokenId -> OracleCommand -> ClientM JSValue
oracleCmd wallet tk = \case
    OracleTokenCommand tokenCommand ->
        tokenCmd
            wallet
            tk
            tokenCommand

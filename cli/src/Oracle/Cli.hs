module Oracle.Cli
    ( OracleCommand (..)
    , oracleCmd
    ) where

import Core.Types (TokenId, Wallet)
import Data.Aeson (Value)
import Oracle.Token.Cli (TokenCommand, tokenCmd)
import Servant.Client (ClientM)

newtype OracleCommand
    = OracleTokenCommand TokenCommand
    deriving (Eq, Show)

oracleCmd :: Wallet -> TokenId -> OracleCommand -> ClientM Value
oracleCmd wallet tk = \case
    OracleTokenCommand tokenCommand ->
        tokenCmd
            wallet
            tk
            tokenCommand

module Oracle.Cli
    ( OracleCommand (..)
    , oracleCmd
    ) where

import Data.Aeson (Value)
import Oracle.Token.Cli (TokenCommand, tokenCmd)
import Servant.Client (ClientM)
import Types (TokenId, Wallet)

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

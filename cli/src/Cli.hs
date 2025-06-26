module Cli
    ( cmd
    , Command (..)
    ) where

import Core.Types
    ( TokenId
    , Wallet
    )
import Oracle.Cli (OracleCommand (..), oracleCmd)
import Servant.Client (ClientM)
import Text.JSON.Canonical (JSValue)
import User.Cli (UserCommand, userCmd)

data Command
    = UserCommand UserCommand
    | OracleCommand OracleCommand
    deriving (Eq, Show)

cmd :: Wallet -> TokenId -> Command -> ClientM JSValue
cmd wallet tk command = do
    case command of
        UserCommand userCommand -> userCmd wallet tk userCommand
        OracleCommand oracleCommand -> oracleCmd wallet tk oracleCommand

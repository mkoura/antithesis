module Cli
    ( cmd
    , Command (..)
    ) where

import Data.Aeson (Value)
import Oracle.Cli (OracleCommand (..), oracleCmd)
import Servant.Client (ClientM)
import Types
    ( TokenId
    , Wallet
    )
import User.Cli (UserCommand, userCmd)

data Command
    = UserCommand UserCommand
    | OracleCommand OracleCommand
    deriving (Eq, Show)

cmd :: Wallet -> TokenId -> Command -> ClientM Value
cmd wallet tk command = do
    case command of
        UserCommand userCommand -> userCmd wallet tk userCommand
        OracleCommand oracleCommand -> oracleCmd wallet tk oracleCommand

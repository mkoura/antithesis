module Cli
    ( cmd
    ) where

import Data.Aeson (Value)
import Oracle.Token.Cli
    ( tokenCmd
    )
import Servant.Client (ClientM)
import Types
    ( Command (..)
    , OracleCommand (..)
    , TokenId
    , Wallet
    )
import User.Cli (userCmd)

cmd :: Wallet -> TokenId -> Command -> ClientM Value
cmd wallet tk command = do
    case command of
        UserCommand userCommand -> userCmd wallet tk userCommand
        OracleCommand oracleCommand ->
            case oracleCommand of
                OracleTokenCommand tokenCommand ->
                    tokenCmd
                        wallet
                        tk
                        tokenCommand

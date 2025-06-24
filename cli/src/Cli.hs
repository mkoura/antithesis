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
    , UserCommand (..)
    , Wallet
    )
import User.Requester.Cli
    ( requesterCmd
    )

cmd :: Wallet -> TokenId -> Command -> ClientM Value
cmd wallet tk command = do
    case command of
        UserCommand userCommand ->
            case userCommand of
                UserRequesterCommand requesterCommand ->
                    requesterCmd wallet tk requesterCommand
        OracleCommand oracleCommand ->
            case oracleCommand of
                OracleTokenCommand tokenCommand ->
                    tokenCmd
                        wallet
                        tk
                        tokenCommand

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

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
    , UserCommand (..)
    )
import User.Requester.Cli
    ( requesterCmd
    )

cmd :: Command -> ClientM Value
cmd command = do
    case command of
        UserCommand userCommand ->
            case userCommand of
                UserRequesterCommand requesterCommand -> requesterCmd requesterCommand
        OracleCommand oracleCommand ->
            case oracleCommand of
                OracleTokenCommand tokenCommand -> tokenCmd tokenCommand

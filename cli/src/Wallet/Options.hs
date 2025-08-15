{-# LANGUAGE DerivingStrategies #-}

module Wallet.Options
    ( walletCommandParser
    ) where

import Lib.Box (Box (..))
import OptEnvConf
    ( Parser
    , command
    , commands
    )
import Wallet.Cli (WalletCommand (..))

walletCommandParser :: Parser (Box WalletCommand)
walletCommandParser =
    commands
        [ command
            "create"
            "Create a new wallet"
            $ pure (Box Create)
        , command
            "info"
            "Get the wallet information"
            $ pure (Box Info)
        ]

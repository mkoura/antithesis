{-# LANGUAGE DerivingStrategies #-}

module Wallet.Options
    ( walletCommandParser
    ) where

import Core.Options (walletOption)
import Lib.Box (Box (..))
import OptEnvConf
import Wallet.Cli (WalletCommand (..))

walletFileOption :: Parser FilePath
walletFileOption =
    setting
        [ help "File to store the wallet mnemonic"
        , metavar "MNEMONICS"
        , env "ANTI_WALLET_FILE"
        , reader str
        ]

walletCommandParser :: Parser (Box WalletCommand)
walletCommandParser =
    commands
        [ command
            "create"
            "Create a new wallet"
            $ Box . Create
                <$> walletFileOption
        , command
            "info"
            "Get the wallet information"
            $ Box . Info
                <$> walletOption
        ]

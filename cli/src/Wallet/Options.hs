{-# LANGUAGE DerivingStrategies #-}

module Wallet.Options
    ( walletCommandParser
    ) where

import Core.Options (walletOption)
import Core.Types.Mnemonics.Options
    ( walletFileOption
    , walletPassphraseCommon
    )
import Data.Text (Text)
import Lib.Box (Box (..))
import OptEnvConf
import Wallet.Cli (WalletCommand (..))

walletCommandParser :: Parser (Box WalletCommand)
walletCommandParser =
    commands
        [ command
            "create"
            "Create a new wallet"
            $ fmap Box . Create
                <$> walletFileOption
                <*> passphraseOption
        , command
            "info"
            "Get the wallet information"
            $ Box . Info
                <$> walletOption
        ]

passphraseOption :: Parser (Maybe Text)
passphraseOption = optional walletPassphraseCommon

{-# LANGUAGE DerivingStrategies #-}

module Wallet.Options
    ( walletCommandParser
    ) where

import Core.Options (walletOption)
import Core.Types.Mnemonics.Options
    ( walletFileOption
    , walletFileArgOption
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
        , command
            "decrypt"
            "Decrypt the wallet's secrets, write it to the specified file and set it as ANTI_WALLET_FILE"
            $ fmap Box . Decrypt
                <$> walletOption
                <*> walletFileArgOption
        , command
            "encrypt"
            "Encrypt the wallet's secrets, write it to the specified file and set it as ANTI_WALLET_FILE"
            $ fmap Box . Encrypt
                <$> walletOption
                <*> walletFileArgOption
        ]

passphraseOption :: Parser (Maybe Text)
passphraseOption = optional walletPassphraseCommon

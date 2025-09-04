{-# LANGUAGE OverloadedRecordDot #-}

module Wallet.Cli
    ( walletCmd
    , WalletCommand (..)
    ) where

import Control.Monad (replicateM)
import Core.Types.Basic (Address, Owner)
import Core.Types.Mnemonics
    ( Mnemonics (..)
    )
import Core.Types.Wallet (Wallet (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Lib.JSON.Canonical.Extra (object, (.=))
import Submitting (walletFromMnemonic, writeWallet)
import System.Random (randomRIO)
import Text.JSON.Canonical
    ( JSValue (JSString)
    , ToJSON (..)
    )
import Words (englishWords)

data WalletError
    = WalletPresent
    | WalletMissing
    | WalletAlreadyDecrypted
    | WalletAlreadyEncrypted

instance Applicative m => ToJSON m WalletError where
    toJSON WalletPresent = pure $ JSString "Wallet is present"
    toJSON WalletMissing = pure $ JSString "Wallet is missing"
    toJSON WalletAlreadyDecrypted = pure $ JSString "Wallet's file is already decrypted"
    toJSON WalletAlreadyEncrypted = pure $ JSString "Wallet's file is already encrypted"

instance (ToJSON m a, Monad m) => ToJSON m (Either WalletError a) where
    toJSON (Right a) = toJSON a
    toJSON (Left e) = object ["error" .= e]

data WalletInfo = WalletInfo
    { address :: Address
    , owner :: Owner
    , encryptedInfo :: Bool
    }

instance Monad m => ToJSON m WalletInfo where
    toJSON WalletInfo{address, owner, encryptedInfo} =
        object
            [ "address" .= address
            , "owner" .= owner
            , "encrypted"
                .= if encryptedInfo
                    then JSString "yes"
                    else JSString "no"
            ]

data WalletCommand a where
    Info
        :: Wallet -> WalletCommand (Either WalletError WalletInfo)
    Create
        :: FilePath
        -> Maybe Text
        -> WalletCommand (Either WalletError WalletInfo)
    Decrypt
        :: Wallet
        -> FilePath
        -> WalletCommand (Either WalletError WalletInfo)
    Encrypt
        :: Wallet
        -> Text -- passphrase
        -> FilePath
        -> WalletCommand (Either WalletError WalletInfo)

deriving instance Show (WalletCommand a)
deriving instance Eq (WalletCommand a)

walletCmd :: WalletCommand a -> IO a
walletCmd (Info wallet) =
    pure
        $ Right
        $ WalletInfo
            { address = wallet.address
            , owner = wallet.owner
            , encryptedInfo = wallet.encrypted
            }
walletCmd (Create walletFile passphrase) = do
    w12 <- replicateM 12 $ element englishWords
    let mnemonics = ClearText $ T.unwords w12
    case walletFromMnemonic True mnemonics of
        Left _e -> walletCmd (Create walletFile passphrase)
        Right wallet -> do
            writeWallet walletFile mnemonics passphrase
            return
                $ Right
                $ WalletInfo
                    { address = wallet.address
                    , owner = wallet.owner
                    , encryptedInfo = isJust passphrase
                    }
walletCmd (Decrypt wallet walletFileDecr) =
    if encrypted wallet
        then do
            writeWallet walletFileDecr (mnemonics wallet) Nothing
            return
                $ Right
                $ WalletInfo
                    { address = wallet.address
                    , owner = wallet.owner
                    , encryptedInfo = False
                    }
        else
            pure $ Left WalletAlreadyDecrypted
walletCmd (Encrypt wallet passphrase walletFileDecr) =
    if encrypted wallet
        then do
            pure $ Left WalletAlreadyEncrypted
        else do
            writeWallet walletFileDecr (mnemonics wallet) (Just passphrase)
            pure
                $ Right
                $ WalletInfo
                    { address = wallet.address
                    , owner = wallet.owner
                    , encryptedInfo = True
                    }

element :: [a] -> IO a
element xs = do
    idx <- randomRIO (0, length xs - 1)
    return $ xs !! idx

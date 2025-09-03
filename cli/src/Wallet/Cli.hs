{-# LANGUAGE OverloadedRecordDot #-}

module Wallet.Cli
    ( walletCmd
    , WalletCommand (..)
    ) where

import Control.Monad (replicateM)
import Core.Encryption (decryptText)
import Core.Types.Basic (Address, Owner)
import Core.Types.Wallet (Wallet (..))
import Data.Text (Text)
import Lib.JSON.Canonical.Extra (object, (.=))
import Submitting (walletFromMnemonic, writeWallet)
import System.Environment (lookupEnv, setEnv)
import System.Random (randomRIO)
import Text.JSON.Canonical
    ( JSValue (JSString)
    , ToJSON (..)
    , toJSString
    )
import Words (englishWords)

import Data.Text qualified as T
import Data.Text.IO qualified as T

data WalletError
    = WalletPresent
    | WalletMissing
    | WalletAlreadyDecrypted
    | WalletAlreadyEncrypted
    | WalletUnexpectedErrror String

instance Applicative m => ToJSON m WalletError where
    toJSON WalletPresent = pure $ JSString "Wallet is present"
    toJSON WalletMissing = pure $ JSString "Wallet is missing"
    toJSON WalletAlreadyDecrypted = pure $ JSString "Wallet's file is already decrypted"
    toJSON WalletAlreadyEncrypted = pure $ JSString "Wallet's file is already encrypted"
    toJSON (WalletUnexpectedErrror err) =
        pure
            $ JSString
            $ "Wallet cmd encountered unexpected situation: " <> toJSString err

instance (ToJSON m a, Monad m) => ToJSON m (Either WalletError a) where
    toJSON (Right a) = toJSON a
    toJSON (Left e) = object ["error" .= e]

data WalletInfo = WalletInfo
    { address :: Address
    , owner :: Owner
    }

instance Monad m => ToJSON m WalletInfo where
    toJSON WalletInfo{address, owner} =
        object
            [ "address" .= address
            , "owner" .= owner
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
            }
walletCmd (Create walletFile passphrase) = do
    w12 <- replicateM 12 $ element englishWords
    case walletFromMnemonic True w12 of
        Left _e -> walletCmd (Create walletFile passphrase)
        Right wallet -> do
            writeWallet walletFile w12 passphrase
            return
                $ Right
                $ WalletInfo
                    { address = wallet.address
                    , owner = wallet.owner
                    }
walletCmd (Decrypt wallet walletFileDecr) =
    if encrypted wallet
        then do
            lookupEnv "ANTI_WALLET_PASSPHRASE" >>= \case
                Nothing ->
                    pure
                        $ Left
                        $ WalletUnexpectedErrror
                            "passphrase should be written in 'ANTI_WALLET_PASSPHRASE' environment variable"
                Just passphrase ->
                    lookupEnv "ANTI_WALLET_FILE" >>= \case
                        Nothing ->
                            pure
                                $ Left
                                $ WalletUnexpectedErrror
                                    "file path should be written in 'ANTI_WALLET_FILE' environment variable"
                        Just filepath -> do
                            mnemonicsEnc <- T.readFile filepath
                            case decryptText (T.pack passphrase) mnemonicsEnc of
                                Left err ->
                                    pure
                                        $ Left
                                        $ WalletUnexpectedErrror
                                        $ "mnemonic decryption error. In detail, " ++ err
                                Right mnemonicsDecrRaw -> do
                                    let mnemonicsDecr = T.words mnemonicsDecrRaw
                                    writeWallet walletFileDecr mnemonicsDecr Nothing
                                    setEnv "ANTI_WALLET_FILE" walletFileDecr
                                    return
                                        $ Right
                                        $ WalletInfo
                                            { address = wallet.address
                                            , owner = wallet.owner
                                            }
        else
            pure $ Left WalletAlreadyDecrypted
walletCmd (Encrypt wallet walletFileDecr) =
    if encrypted wallet
        then do
            pure $ Left WalletAlreadyEncrypted
        else
            lookupEnv "ANTI_WALLET_PASSPHRASE" >>= \case
                Nothing ->
                    pure
                        $ Left
                        $ WalletUnexpectedErrror
                            "passphrase should be written in 'ANTI_WALLET_PASSPHRASE' environment variable"
                Just passphrase ->
                    lookupEnv "ANTI_WALLET_FILE" >>= \case
                        Nothing ->
                            pure
                                $ Left
                                $ WalletUnexpectedErrror
                                    "file path should be written in 'ANTI_WALLET_FILE' environment variable"
                        Just filepath -> do
                            mnemonics <- T.words <$> T.readFile filepath
                            writeWallet walletFileDecr mnemonics (Just $ T.pack passphrase)
                            setEnv "ANTI_WALLET_FILE" walletFileDecr
                            return
                                $ Right
                                $ WalletInfo
                                    { address = wallet.address
                                    , owner = wallet.owner
                                    }

element :: [a] -> IO a
element xs = do
    idx <- randomRIO (0, length xs - 1)
    return $ xs !! idx

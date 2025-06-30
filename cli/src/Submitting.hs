{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Submitting
    ( submitting
    , readWallet
    , walletFromMnemonic
    )
where

import Cardano.Address (bech32)
import Cardano.Address.Derivation
    ( Depth (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , genMasterKeyFromMnemonic
    )
import Cardano.Address.Style.Shelley
    ( Credential (PaymentFromExtendedKey)
    , Role (UTxOExternal)
    , Shelley
    , getKey
    , mkNetworkDiscriminant
    , paymentAddress
    )
import Cardano.Crypto.DSIGN.Class qualified as Crypto
import Cardano.Crypto.Util qualified as Crypto
import Cardano.Crypto.Wallet (XPrv)
import Cardano.Crypto.Wallet qualified as Crypto.HD
import Cardano.Ledger.Api
    ( ConwayEra
    , EraTx (Tx)
    , KeyRole (..)
    , WitVKey (..)
    , addrTxWitsL
    , eraProtVerLow
    , txIdTx
    , witsTxL
    )
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Api.Tx.In (TxId (..))
import Cardano.Ledger.Binary
    ( DecCBOR (decCBOR)
    , DecoderError
    , decodeFullAnnotator
    )
import Cardano.Ledger.Binary.Encoding qualified as Ledger
import Cardano.Ledger.Core
    ( extractHash
    )
import Cardano.Ledger.Keys
    ( VKey (..)
    , asWitness
    )
import Cardano.Mnemonic
import Control.Exception (Exception, throwIO)
import Control.Lens ((%~))
import Control.Monad.IO.Class (MonadIO (..))
import Core.Types
    ( Address (..)
    , SignTxError (..)
    , SignedTx (..)
    , UnsignedTx (..)
    , Wallet (..)
    , WithTxHash (WithTxHash)
    , WithUnsignedTx (..)
    )
import Data.Aeson
    ( FromJSON
    , ToJSON
    , eitherDecodeFileStrict'
    )
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import MPFS.API (submitTransaction)
import Servant.Client (ClientM)
import Text.JSON.Canonical (JSValue)

submitting
    :: Wallet
    -> (Address -> ClientM (WithUnsignedTx JSValue))
    -> ClientM (WithTxHash JSValue)
submitting Wallet{address, sign} action = do
    WithUnsignedTx unsignedTx value <- action address
    case sign $ UnsignedTx unsignedTx of
        Right (SignedTx signedTx) -> do
            txHash <- submitTransaction $ SignedTx signedTx
            return $ WithTxHash txHash value
        Left e -> liftIO $ throwIO e

data WalletDB = WalletDB
    { mnemonic :: [Text]
    , address :: Text
    }
    deriving (Eq, Generic)

instance FromJSON WalletDB
instance ToJSON WalletDB

data WalletError
    = InvalidMnemonic String
    | InvalidWalletFile String
    deriving (Show, Eq)

instance Exception WalletError

readWallet
    :: FilePath
    -> IO Wallet
readWallet walletFile = do
    WalletDB{mnemonic} <-
        either (throwIO . InvalidWalletFile) pure
            =<< eitherDecodeFileStrict' walletFile
    either throwIO pure $ walletFromMnemonic mnemonic

walletFromMnemonic :: [Text] -> Either WalletError Wallet
walletFromMnemonic mnemonicWords = do
    mnemonic <-
        either (Left . InvalidMnemonic . show) Right
            $ mkSomeMnemonic @'[9, 12, 15, 18, 24] mnemonicWords

    let
        rootXPrv :: Shelley 'RootK XPrv
        rootXPrv = genMasterKeyFromMnemonic mnemonic mempty

        accXPrv :: Shelley 'AccountK XPrv
        accXPrv = deriveAccountPrivateKey rootXPrv minBound

        addrXPrv :: Shelley 'PaymentK XPrv
        addrXPrv = deriveAddressPrivateKey accXPrv UTxOExternal minBound
        addrXPub = Crypto.HD.toXPub <$> addrXPrv

        tag = either (error . show) id $ mkNetworkDiscriminant 0
        addr =
            Address
                $ bech32
                $ paymentAddress tag
                $ PaymentFromExtendedKey addrXPub
    pure $ Wallet addr (signTx $ getKey addrXPrv)

signTx :: XPrv -> UnsignedTx -> Either SignTxError SignedTx
signTx xprv (UnsignedTx unsignedHex) = do
    rawBytes <- unhex unsignedHex
    tx <- first (const InvalidTx) (deserializeTx rawBytes)

    let signedLedgerTx = signLedgerTx xprv tx

    pure $ SignedTx $ hex $ serializeTx signedLedgerTx
  where
    hex :: ByteString -> Text
    hex = T.decodeUtf8 . Base16.encode

    unhex :: Text -> Either SignTxError ByteString
    unhex = first (const InvalidHex) . Base16.decode . T.encodeUtf8

serializeTx
    :: Tx ConwayEra
    -> ByteString
serializeTx = BL.toStrict . Ledger.serialize (eraProtVerLow @ConwayEra)

deserializeTx
    :: ByteString
    -> Either DecoderError (Tx ConwayEra)
deserializeTx
    bytes = case decodeFullAnnotator
        (eraProtVerLow @ConwayEra)
        "ConwayTx"
        decCBOR
        $ BL.fromStrict bytes of
        Left err -> error $ "Failed to decode full CBOR: " ++ show err
        Right tx' -> Right tx'

signLedgerTx :: XPrv -> L.Tx L.ConwayEra -> L.Tx L.ConwayEra
signLedgerTx xprv tx =
    addKeyWitnesses tx
        $ Set.fromList
            [ mkKeyWitness (txIdTx tx) xprv
            ]

mkKeyWitness :: TxId -> XPrv -> WitVKey 'Witness
mkKeyWitness (TxId hash) xprv =
    WitVKey
        (getShelleyKeyWitnessVerificationKey xprv)
        ( fromXSignature
            $ Crypto.HD.sign
                BS.empty -- passphrase for (unused) in-memory encryption
                xprv
                (Crypto.getSignableRepresentation $ extractHash hash)
        )
  where
    fromXSignature =
        Crypto.SignedDSIGN
            . fromMaybe (error "impossible: rawDeserialiseSigDSIGN failed")
            . Crypto.rawDeserialiseSigDSIGN
            . Crypto.HD.unXSignature

    getShelleyKeyWitnessVerificationKey
        :: XPrv
        -> VKey Witness
    getShelleyKeyWitnessVerificationKey = asWitness . convert . Crypto.HD.toXPub
      where
        convert =
            VKey
                . fromMaybe impossible
                . Crypto.rawDeserialiseVerKeyDSIGN
                . Crypto.HD.xpubPublicKey
          where
            impossible =
                error "rawDeserialiseVerKeyDSIGN failed converting XPub"

addKeyWitnesses
    :: EraTx era => Tx era -> Set (WitVKey 'Witness) -> Tx era
addKeyWitnesses tx newWits = tx & witsTxL . addrTxWitsL %~ Set.union newWits

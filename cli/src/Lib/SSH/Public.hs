module Lib.SSH.Public
    ( decodePublicKey
    ) where

import Control.Monad (when)
import Core.Types (PublicKeyHash (..))
import Crypto.Error
    ( CryptoFailable (..)
    )
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Binary.Get (Get, getByteString, getWord32be, runGet)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BL

parseEd25519Key :: Get B.ByteString
parseEd25519Key = do
    keyTypeLen <- getWord32be
    keyType <- getByteString (fromIntegral keyTypeLen)
    when (keyType /= "ssh-ed25519") $ fail "Expected ssh-ed25519 key type"

    keyLen <- getWord32be
    when (keyLen /= 32) $ fail "Expected 32-byte Ed25519 public key"
    getByteString 32

decodePublicKey :: PublicKeyHash -> Maybe Ed25519.PublicKey
decodePublicKey (PublicKeyHash pk) =
    let
        base64Decoded = BL.fromStrict $ Base64.decodeLenient $ B.pack pk
        keyBytes = runGet parseEd25519Key base64Decoded
    in
        case Ed25519.publicKey keyBytes of
            CryptoPassed pubKey -> Just pubKey
            CryptoFailed _ -> Nothing

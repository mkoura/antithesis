module Lib.EdToCurve
    ( ed25519ToCurve25519SecretKey
    , ed25519ToCurve25519PublicKey
    ) where

import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (Digest, SHA512 (..), hashWith)
import Crypto.Number.ModArithmetic (expFast)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import Crypto.PubKey.Curve25519 qualified as Curve25519
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Bits (Bits (shiftL, (.&.), (.|.)))
import Data.ByteArray (convert)
import Data.ByteArray qualified as B hiding (append)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)

-- | Inverse modular p
modpInv :: Integer -> Integer
modpInv x = expFast x (p - 2) p

-- Prime for Curve25519/Ed25519 field
p :: Integer
p = (1 `shiftL` 255) - 19

mask255 :: Integer
mask255 = (1 `shiftL` 255) - 1

-- Convert Ed25519 public key (32 bytes) -> X25519 public key (32 bytes)
edPubToX25519 :: ByteString -> Either String ByteString
edPubToX25519 edpub =
    let full = os2ip . B.reverse $ edpub
        y = full .&. mask255
        num = (1 + y) `mod` p
        den = (1 - y) `mod` p
    in  if den == 0
            then Left "edPubToX25519: invalid point (1 - y == 0)"
            else
                let u = (num * modpInv den) `mod` p
                in  Right (B.reverse . i2ospOf_ 32 $ u)

-- Clamp 32-byte scalar for X25519
clampScalar :: ByteString -> ByteString
clampScalar s =
    let b0 = B.index s 0 .&. 248
        b31 = (B.index s 31 .&. 127) .|. 64
        middle = B.take 30 (B.drop 1 s)
    in  B.concat [B.singleton b0, middle, B.singleton b31]

-- Convert Ed25519 32-byte seed -> X25519 scalar (32 bytes)
edSeedToX25519Scalar :: ByteString -> Either String ByteString
edSeedToX25519Scalar seed =
    let d :: Digest SHA512
        d = hashWith SHA512 seed
        full = BA.convert d :: ByteString -- 64 bytes
        scalar' = B.take 32 full
    in  Right (clampScalar scalar')

ed25519ToCurve25519SecretKey
    :: Ed25519.SecretKey -> Either String Curve25519.SecretKey
ed25519ToCurve25519SecretKey sk =
    case edSeedToX25519Scalar (convert sk) of
        Left err -> Left err
        Right scalar -> case Curve25519.secretKey scalar of
            CryptoFailed err -> Left (show err)
            CryptoPassed sk' -> Right sk'

ed25519ToCurve25519PublicKey
    :: Ed25519.PublicKey -> Either String Curve25519.PublicKey
ed25519ToCurve25519PublicKey pk =
    case edPubToX25519 (convert pk) of
        Left err -> Left err
        Right u -> case Curve25519.publicKey u of
            CryptoFailed err -> Left (show err)
            CryptoPassed pk' -> Right pk'

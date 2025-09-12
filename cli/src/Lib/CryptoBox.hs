module Lib.CryptoBox
    ( Nonce192 (..)
    , encrypt
    , decrypt
    , mkNonce
    )
where

import Crypto.Box (create, open)
import Crypto.PubKey.Ed25519 (PublicKey, SecretKey)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Lib.EdToCurve (convertPublicKey, convertSecretKey)

newtype Nonce192 = Nonce192 ByteString
    deriving (Show, Eq)

mkNonce :: ByteString -> Maybe Nonce192
mkNonce bs
    | B.length bs >= 24 = Just (Nonce192 $ B.take 24 bs)
    | otherwise = Nothing

encrypt
    :: PublicKey
    -- ^ Recipient's public key
    -> SecretKey
    -- ^ Sender's secret key
    -> ByteString
    -- ^ Plaintext message
    -> Nonce192
    -- ^ nonce
    -> Either String ByteString
encrypt pk sk msg (Nonce192 nonce) = do
    pk' <- convertPublicKey pk
    sk' <- convertSecretKey sk
    Right $ create msg nonce pk' sk'

decrypt
    :: PublicKey
    -- ^ Sender's public key
    -> SecretKey
    -- ^ Recipient's secret key
    -> ByteString
    -- ^ Ciphertext message
    -> Nonce192
    -- ^ nonce
    -> Either String (Maybe ByteString)
decrypt pk sk msg (Nonce192 nonce) = do
    pk' <- convertPublicKey pk
    sk' <- convertSecretKey sk
    Right $ open msg nonce pk' sk'

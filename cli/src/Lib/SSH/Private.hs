{-
This module was taken almost verbatim from the SSH library for Haskell.
https://hackage.haskell.org/package/hssh-0.1.0.0/docs/src/Network.SSH.Key.html
-}
{-# LANGUAGE MultiWayIf #-}

module Lib.SSH.Private
    ( decodePrivateSSHFile
    , Sign
    , KeyAPI (..)
    , SSHKeySelector (..)
    , SigningMap
    ) where

import Control.Applicative (many, (<|>))
import Control.Monad (replicateM, void, when)
import Crypto.Cipher.AES qualified as Cipher
import Crypto.Cipher.Types qualified as Cipher
import Crypto.Error
    ( CryptoError
        ( CryptoError_IvSizeInvalid
        , CryptoError_KeySizeInvalid
        )
    , CryptoFailable (..)
    )
import Crypto.KDF.BCryptPBKDF qualified as BCryptPBKDF
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.ByteArray qualified as BA
import Data.ByteArray.Parse
    ( Parser
    , Result (ParseFail, ParseMore, ParseOK)
    , anyByte
    , byte
    , bytes
    , hasMore
    , parse
    , skip
    )
import Data.ByteArray.Parse qualified as P
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Map.Strict qualified as Map
import Data.String (IsString)
import Data.Word (Word32, Word8)

newtype SSHKeySelector = SSHKeySelector
    { sshKeySelector :: String
    }
    deriving (Eq, Show, IsString, Ord)

type Sign = B.ByteString -> Ed25519.Signature

data KeyAPI = KeyAPI
    { sign :: Sign
    , publicKey :: Ed25519.PublicKey
    }
type SigningMap = SSHKeySelector -> Maybe KeyAPI

decodePrivateSSHFile
    :: B.ByteString
    -> FilePath
    -> IO (SSHKeySelector -> Maybe KeyAPI)
decodePrivateSSHFile passphrase filePath = do
    content <- B.readFile filePath
    ks <- decodePrivateKeyFile passphrase content
    let mkSign (KeyPairEd25519 pk sk, comment) =
            let k = SSHKeySelector $ B.unpack comment
            in  Map.singleton k
                    $ KeyAPI
                        { sign = Ed25519.sign sk pk
                        , publicKey = pk
                        }
    pure $ flip Map.lookup $ foldMap mkSign ks

data KeyPair
    = KeyPairEd25519 Ed25519.PublicKey Ed25519.SecretKey
    deriving (Eq, Show)

decodePrivateKeyFile
    :: MonadFail m
    => ByteString
    -> ByteString
    -> m [(KeyPair, ByteString)]
decodePrivateKeyFile passphrase =
    f . parse (parsePrivateKeyFile passphrase) . BA.convert
  where
    f (ParseOK _ a) = pure a
    f (ParseFail e) = fail e
    f (ParseMore c) = f (c Nothing)

parsePrivateKeyFile
    :: ByteString
    -> Parser ByteString [(KeyPair, ByteString)]
parsePrivateKeyFile passphrase = do
    bytes "-----BEGIN OPENSSH PRIVATE KEY-----"
    void $ many space
    bs <- parseBase64
    void $ many space
    bytes "-----END OPENSSH PRIVATE KEY-----"
    void $ many space
    hasMore >>= flip when syntaxError
    case parse (parseKeys passphrase) bs of
        ParseOK _ keys -> pure keys
        ParseFail e -> fail e
        ParseMore _ -> syntaxError

syntaxError :: Parser ByteString a
syntaxError = fail "Syntax error"

parseBase64 :: Parser ByteString ByteString
parseBase64 = s0 []
  where
    -- Initial state and final state.
    s0 xs =
        (char >>= s1 xs)
            <|> (space1 >> s0 xs)
            <|> pure (BA.pack $ reverse xs)
    -- One character read (i). Three more characters or whitespace expected.
    s1 xs i =
        (char >>= s2 xs i) <|> (space1 >> s1 xs i)
    -- Two characters read (i and j). Either '==' or space or two more character expected.
    s2 xs i j =
        r2 xs i j <|> (char >>= s3 xs i j) <|> (space1 >> s2 xs i j)
    -- Three characters read (i, j and k). Either a '=' or space or one more character expected.
    s3 xs i j k =
        r3 xs i j k <|> (char >>= s4 xs i j k) <|> (space1 >> s3 xs i j k)
    -- Four characters read (i, j, k and l). Computation of result and transition back to s0.
    s4 xs i j k l = s0 $ byte3 : byte2 : byte1 : xs
      where
        byte1 = (i `shiftL` 2) + (j `shiftR` 4)
        byte2 = ((j .&. 15) `shiftL` 4) + (k `shiftR` 2)
        byte3 = ((k .&. 3) `shiftL` 6) + l
    -- Read two '=' chars as finalizer. Only valid from state s2.
    r2 xs i j = padding >> padding >> pure (BA.pack $ reverse $ byte1 : xs)
      where
        byte1 = (i `shiftL` 2) + (j `shiftR` 4)
    -- Read one '=' char as finalizer. Only valid from state s1.
    r3 xs i j k = padding >> pure (BA.pack $ reverse $ byte2 : byte1 : xs)
      where
        byte1 = (i `shiftL` 2) + (j `shiftR` 4)
        byte2 = ((j .&. 15) `shiftL` 4) + (k `shiftR` 2)

    char =
        anyByte >>= \c ->
            if
                | c >= fe 'A' && c <= fe 'Z' -> pure (c - fe 'A')
                | c >= fe 'a' && c <= fe 'z' -> pure (c - fe 'a' + 26)
                | c >= fe '0' && c <= fe '9' -> pure (c - fe '0' + 52)
                | c == fe '+' -> pure 62
                | c == fe '/' -> pure 63
                | otherwise -> fail ""

    padding = byte 61 -- 61 == fromEnum '='
fe :: Char -> Word8
fe = fromIntegral . fromEnum

space :: Parser ByteString ()
space =
    anyByte >>= \c ->
        if
            | c == fe ' ' -> pure ()
            | c == fe '\n' -> pure ()
            | c == fe '\r' -> pure ()
            | c == fe '\t' -> pure ()
            | otherwise -> fail ""

space1 :: Parser ByteString ()
space1 = space >> many space >> pure ()

getWord32be :: Parser ByteString Word32
getWord32be = do
    x0 <- fromIntegral <$> anyByte
    x1 <- fromIntegral <$> anyByte
    x2 <- fromIntegral <$> anyByte
    x3 <- fromIntegral <$> anyByte
    pure $ shiftR x0 24 .|. shiftR x1 16 .|. shiftR x2 8 .|. x3

getString :: Parser ByteString ByteString
getString = P.take . fromIntegral =<< getWord32be

parseKeys
    :: ()
    => ByteString
    -> Parser ByteString [(KeyPair, ByteString)]
parseKeys passphrase = do
    bytes "openssh-key-v1\NUL"
    cipherAlgo <- getString
    kdfAlgo <- getString
    skip 4 -- size of the kdf section
    deriveKey <- case kdfAlgo of
        "none" ->
            pure $ \_ -> CryptoFailed CryptoError_KeySizeInvalid
        "bcrypt" -> do
            salt <- getString
            rounds <- fromIntegral <$> getWord32be
            pure $ \case
                Cipher.KeySizeFixed len ->
                    CryptoPassed
                        $ BCryptPBKDF.generate
                            (BCryptPBKDF.Parameters rounds len)
                            passphrase
                            salt
                _ -> undefined -- impossible
        _ ->
            fail
                $ "Unsupported key derivation function "
                    ++ B.unpack kdfAlgo

    numberOfKeys <- fromIntegral <$> getWord32be
    _publicKeysRaw <- getString -- not used
    privateKeysRawEncrypted <- getString
    privateKeysRawDecrypted <-
        BA.convert <$> case cipherAlgo of
            "none" -> pure privateKeysRawEncrypted
            "aes256-cbc" -> decryptPrivateKeys deriveKey
                $ \cipher iv ->
                    Cipher.cbcDecrypt
                        cipher
                        iv
                        privateKeysRawEncrypted
            "aes256-ctr" -> decryptPrivateKeys deriveKey
                $ \cipher iv ->
                    Cipher.ctrCombine
                        cipher
                        iv
                        privateKeysRawEncrypted
            _ -> fail $ "Unsupported cipher " ++ show cipherAlgo
    case parse (parsePrivateKeys numberOfKeys) privateKeysRawDecrypted of
        ParseOK _ keys -> pure keys
        ParseFail e -> fail e
        ParseMore _ -> syntaxError

decryptPrivateKeys
    :: (Cipher.BlockCipher c, MonadFail m)
    => (Cipher.KeySizeSpecifier -> CryptoFailable ByteString)
    -> (Cipher.AES256 -> Cipher.IV c -> b)
    -> m b
decryptPrivateKeys deriveKey how = do
    let result = case Cipher.cipherKeySize (undefined :: Cipher.AES256) of
            Cipher.KeySizeFixed keySize -> do
                let ivSize = Cipher.blockSize (undefined :: Cipher.AES256)
                keyIV <- deriveKey $ Cipher.KeySizeFixed (keySize + ivSize)
                let key = BA.take keySize keyIV
                case Cipher.makeIV (BA.drop keySize keyIV) of
                    Nothing -> CryptoFailed CryptoError_IvSizeInvalid
                    Just iv -> do
                        cipher <- Cipher.cipherInit key
                        pure $ how cipher iv
            _ -> CryptoFailed CryptoError_KeySizeInvalid
    case result of
        CryptoPassed a -> pure a
        CryptoFailed e -> fail (show e)

parsePrivateKeys
    :: Int
    -> Parser ByteString [(KeyPair, ByteString)]
parsePrivateKeys count = do
    check1 <- getWord32be
    check2 <- getWord32be
    when (check1 /= check2) (fail "Unsuccessful decryption")
    replicateM count $ do
        key <-
            getString >>= \algo -> case algo of
                "ssh-ed25519" -> do
                    skip 3
                    byte 32 -- length field (is always 32 for ssh-ed25519)
                    skip Ed25519.publicKeySize
                    skip 3
                    byte 64 -- length field (is always 64 for ssh-ed25519)
                    secretKeyRaw <- P.take 32
                    publicKeyRaw <- P.take 32
                    let key =
                            KeyPairEd25519
                                <$> Ed25519.publicKey publicKeyRaw
                                <*> Ed25519.secretKey secretKeyRaw
                    case key of
                        CryptoPassed a -> pure a
                        CryptoFailed _ ->
                            fail
                                $ "Invalid "
                                    ++ B.unpack algo
                                    ++ " key"
                _ ->
                    fail
                        $ "Unsupported algorithm "
                            ++ B.unpack algo
        comment <- BA.convert <$> getString
        pure (key, comment)

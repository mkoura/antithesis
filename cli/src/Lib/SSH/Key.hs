{-# LANGUAGE MultiWayIf #-}

module Lib.SSH.Key
    ( decodePrivateSSHFile
    , Sign
    , SSHKeySelector (..)
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
import Data.ByteArray.Parse qualified as BP
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString)
import Data.Word (Word32, Word8)

newtype SSHKeySelector = SSHKeySelector
    { sshKeySelector :: String
    }
    deriving (Eq, Show, IsString, Ord)

type Sign = B.ByteString -> Ed25519.Signature

decodePrivateSSHFile
    :: B.ByteString
    -> FilePath
    -> IO (Map SSHKeySelector Sign)
decodePrivateSSHFile passphrase filePath = do
    content <- B.readFile filePath
    ks <- decodePrivateKeyFile passphrase content
    let mkSign (KeyPairEd25519 pk sk, comment) =
            let k = SSHKeySelector $ B.unpack comment
            in  Map.singleton k $ Ed25519.sign sk pk
    pure $ foldMap mkSign ks

data KeyPair
    = KeyPairEd25519 Ed25519.PublicKey Ed25519.SecretKey
    deriving (Eq, Show)

decodePrivateKeyFile
    :: ( MonadFail m
       , BA.ByteArray input
       , BA.ByteArrayAccess passphrase
       , BA.ByteArray comment
       )
    => passphrase
    -> input
    -> m [(KeyPair, comment)]
decodePrivateKeyFile passphrase =
    f . BP.parse (parsePrivateKeyFile passphrase) . BA.convert
  where
    f (BP.ParseOK _ a) = pure a
    f (BP.ParseFail e) = fail e
    f (BP.ParseMore c) = f (c Nothing)

parsePrivateKeyFile
    :: (BA.ByteArrayAccess passphrase, BA.ByteArray comment)
    => passphrase
    -> BP.Parser BS.ByteString [(KeyPair, comment)]
parsePrivateKeyFile passphrase = do
    BP.bytes "-----BEGIN OPENSSH PRIVATE KEY-----"
    void $ many space
    bs <- parseBase64
    void $ many space
    BP.bytes "-----END OPENSSH PRIVATE KEY-----"
    void $ many space
    BP.hasMore >>= flip when syntaxError
    case BP.parse (parseKeys passphrase) bs of
        BP.ParseOK _ keys -> pure keys
        BP.ParseFail e -> fail e
        BP.ParseMore _ -> syntaxError

syntaxError :: BP.Parser ba a
syntaxError = fail "Syntax error"

parseBase64 :: BA.ByteArray ba => BP.Parser ba ba
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

    char :: (BA.ByteArray ba) => BP.Parser ba Word8
    char =
        BP.anyByte >>= \c ->
            if
                | c >= fe 'A' && c <= fe 'Z' -> pure (c - fe 'A')
                | c >= fe 'a' && c <= fe 'z' -> pure (c - fe 'a' + 26)
                | c >= fe '0' && c <= fe '9' -> pure (c - fe '0' + 52)
                | c == fe '+' -> pure 62
                | c == fe '/' -> pure 63
                | otherwise -> fail ""

    padding :: (BA.ByteArray ba) => BP.Parser ba ()
    padding = BP.byte 61 -- 61 == fromEnum '='
fe :: Char -> Word8
fe = fromIntegral . fromEnum

space :: (BA.ByteArray ba) => BP.Parser ba ()
space =
    BP.anyByte >>= \c ->
        if
            | c == fe ' ' -> pure ()
            | c == fe '\n' -> pure ()
            | c == fe '\r' -> pure ()
            | c == fe '\t' -> pure ()
            | otherwise -> fail ""

space1 :: (BA.ByteArray ba) => BP.Parser ba ()
space1 = space >> many space >> pure ()

getWord32be :: BA.ByteArray ba => BP.Parser ba Word32
getWord32be = do
    x0 <- fromIntegral <$> BP.anyByte
    x1 <- fromIntegral <$> BP.anyByte
    x2 <- fromIntegral <$> BP.anyByte
    x3 <- fromIntegral <$> BP.anyByte
    pure $ shiftR x0 24 .|. shiftR x1 16 .|. shiftR x2 8 .|. x3

getString :: BA.ByteArray ba => BP.Parser ba ba
getString = BP.take . fromIntegral =<< getWord32be

parseKeys
    :: ( BA.ByteArray input
       , IsString input
       , Show input
       , BA.ByteArray comment
       , BA.ByteArrayAccess passphrase
       )
    => passphrase
    -> BP.Parser input [(KeyPair, comment)]
parseKeys passphrase = do
    BP.bytes "openssh-key-v1\NUL"
    cipherAlgo <- getString
    kdfAlgo <- getString
    BP.skip 4 -- size of the kdf section
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
                            (BA.convert passphrase :: BA.Bytes)
                            salt
                _ -> undefined -- impossible
        _ ->
            fail
                $ "Unsupported key derivation function "
                    ++ show (BA.convert kdfAlgo :: BA.Bytes)

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
    case BP.parse (parsePrivateKeys numberOfKeys) privateKeysRawDecrypted of
        BP.ParseOK _ keys -> pure keys
        BP.ParseFail e -> fail e
        BP.ParseMore _ -> syntaxError

decryptPrivateKeys
    :: (Cipher.BlockCipher c, MonadFail m)
    => (Cipher.KeySizeSpecifier -> CryptoFailable BA.ScrubbedBytes)
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
    :: (BA.ByteArray comment)
    => Int
    -> BP.Parser BA.ScrubbedBytes [(KeyPair, comment)]
parsePrivateKeys count = do
    check1 <- getWord32be
    check2 <- getWord32be
    when (check1 /= check2) (fail "Unsuccessful decryption")
    replicateM count $ do
        key <-
            getString >>= \algo -> case algo of
                "ssh-ed25519" -> do
                    BP.skip 3
                    BP.byte 32 -- length field (is always 32 for ssh-ed25519)
                    BP.skip Ed25519.publicKeySize
                    BP.skip 3
                    BP.byte 64 -- length field (is always 64 for ssh-ed25519)
                    secretKeyRaw <- BP.take 32
                    publicKeyRaw <- BP.take 32
                    let key =
                            KeyPairEd25519
                                <$> Ed25519.publicKey publicKeyRaw
                                <*> Ed25519.secretKey secretKeyRaw
                    case key of
                        CryptoPassed a -> pure a
                        CryptoFailed _ ->
                            fail
                                $ "Invalid "
                                    ++ show (BA.convert algo :: BA.Bytes)
                                    ++ " key"
                _ ->
                    fail
                        $ "Unsupported algorithm "
                            ++ show (BA.convert algo :: BA.Bytes)
        comment <- BA.convert <$> getString
        pure (key, comment)

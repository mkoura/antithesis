{-
This module was taken almost verbatim from the SSH library for Haskell.
https://hackage.haskell.org/package/hssh-0.1.0.0/docs/src/Network.SSH.Key.html
-}

module Lib.SSH.Private
    ( decodePrivateSSHFile
    , Sign
    , KeyAPI (..)
    , SSHKeySelector (..)
    , SigningMap
    ) where

import Control.Applicative (many, (<|>))
import Control.Monad (replicateM, void, when, (<=<))
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
import Data.Binary
import Data.Binary.Get
    ( Decoder (..)
    , getByteString
    , getRemainingLazyByteString
    , getWord32be
    , isolate
    , pushChunk
    , runGet
    , runGetIncremental
    , runGetOrFail
    , skip
    )
import Data.Bits (Bits (shiftL, shiftR, (.&.)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.String (IsString)

newtype SSHKeySelector = SSHKeySelector
    { sshKeySelector :: String
    }
    deriving (Eq, Show, IsString, Ord)

type Sign = BC.ByteString -> Ed25519.Signature

data KeyAPI = KeyAPI
    { sign :: Sign
    , publicKey :: Ed25519.PublicKey
    }
type SigningMap = SSHKeySelector -> Maybe KeyAPI

decodePrivateSSHFile
    :: BC.ByteString
    -> FilePath
    -> IO (SSHKeySelector -> Maybe KeyAPI)
decodePrivateSSHFile passphrase filePath = do
    content <- B.readFile filePath
    ks <- decodePrivateKeyFile passphrase content
    let mkSign (KeyPairEd25519 pk sk, comment) =
            let k = SSHKeySelector $ BC.unpack comment
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
    pure . runGet (parsePrivateKeyFile passphrase) . BL.fromStrict

parsePrivateKeyFile
    :: ByteString
    -> Get [(KeyPair, ByteString)]
parsePrivateKeyFile passphrase = do
    matchBytes "-----BEGIN OPENSSH PRIVATE KEY-----"
    void $ many space
    keys <- feedingBase64 $ parseKeys passphrase
    void $ many space
    matchBytes "-----END OPENSSH PRIVATE KEY-----"
    void $ many space
    pure keys

fromDecoder :: Decoder a -> Get a
fromDecoder (Done _ _ a) = pure a
fromDecoder (Partial _f) = fail "Unexpected end of input"
fromDecoder (Fail _ _ err) = fail err

feedingBase64 :: Get a -> Get a
feedingBase64 = fromDecoder <=< s0 . runGetIncremental
  where
    continue d c = pure $ pushChunk d (B.pack c)
    b1 i j = (i `shiftL` 2) + (j `shiftR` 4)
    b2 j k = ((j .&. 15) `shiftL` 4) + (k `shiftR` 2)
    b3 k l = ((k .&. 3) `shiftL` 6) + l
    wc f = getWord8 >>= mapChar >>= f
    ws f = space1 >> f
    -- Initial state and final state.
    s0 xs = wc (s1 xs) <|> ws (s0 xs) <|> pure xs
    -- One character read (i). Three more characters or whitespace expected.
    s1 xs i = wc (s2 xs i) <|> ws (s1 xs i)
    -- Two characters read (i and j). Either '==' or space or two more character expected.
    s2 xs i j = r2 xs i j <|> wc (s3 xs i j) <|> ws (s2 xs i j)
    -- Three characters read (i, j and k). Either a '=' or space or one more character expected.
    s3 xs i j k = r3 xs i j k <|> wc (s4 xs i j k) <|> ws (s3 xs i j k)
    -- Four characters read (i, j, k and l). Computation of result and transition back to s0.
    s4 xs i j k l = continue xs [b1 i j, b2 j k, b3 k l] >>= s0
    -- Read two '=' chars as finalizer. Only valid from state s2.
    r2 xs i j = end >> end >> continue xs [b1 i j]
    -- Read one '=' char as finalizer. Only valid from state s1.
    r3 xs i j k = end >> continue xs [b1 i j, b2 j k]
    end = byte 61 -- '='

mapChar :: Word8 -> Get Word8
mapChar c
    | c == 43 = pure 62 -- '+'
    | c == 47 = pure 63 -- '/'
    | c >= 48 && c <= 57 = pure $ c - 48 + 52 -- '0'..'9'
    | c >= 65 && c <= 90 = pure $ c - 65 -- 'A'..'Z'
    | c >= 97 && c <= 122 = pure $ c - 97 + 26 -- 'a'..'z'
    | otherwise = fail "Invalid base64 character"

byte :: Word8 -> Get ()
byte b =
    getWord8 >>= \c ->
        if c == b
            then pure ()
            else fail $ "Expected byte " ++ show b ++ ", got " ++ show c

isSpace :: Word8 -> Bool
isSpace c = c == 32 || c == 10 || c == 13 || c == 9

space :: Get ()
space =
    getWord8 >>= \c ->
        if isSpace c
            then pure ()
            else fail $ "Expected space, got " ++ show c

space1 :: Get ()
space1 = space >> many space >> pure ()

getChunk :: Get ByteString
getChunk = getByteString . fromIntegral =<< getWord32be

matchBytes :: ByteString -> Get ()
matchBytes bs = do
    let len = B.length bs
    when (len == 0) (fail "Empty byte string")
    bs' <- getByteString len
    when
        (bs /= bs')
        (fail $ "Expected " ++ show bs ++ ", got " ++ show bs')

embeddingDecryption
    :: Get a -- to run on decrypted
    -> (ByteString -> Get ByteString) -- decryptor
    -> Get a
embeddingDecryption p f = do
    len <- getWord32be
    s <- isolate (fromIntegral len) $ f =<< rest
    case runGetOrFail p $ BL.fromStrict s of
        Left (_, _, err) -> fail err
        Right (_, _, a) -> pure a

rest :: Get B.StrictByteString
rest = BL.toStrict <$> getRemainingLazyByteString

parseKeys
    :: ()
    => ByteString
    -> Get [(KeyPair, ByteString)]
parseKeys passphrase = do
    matchBytes "openssh-key-v1\NUL"
    cipherAlgo <- getChunk
    kdfAlgo <- getChunk
    skip 4 -- size of the kdf section
    deriveKey <- case kdfAlgo of
        "bcrypt" -> do
            salt <- getChunk
            rounds <- fromIntegral <$> getWord32be
            pure $ \case
                Cipher.KeySizeFixed len ->
                    CryptoPassed
                        $ BCryptPBKDF.generate
                            (BCryptPBKDF.Parameters rounds len)
                            passphrase
                            salt
                _ -> CryptoFailed CryptoError_KeySizeInvalid
        _ ->
            fail
                $ "Unsupported key derivation function "
                    ++ BC.unpack kdfAlgo

    numberOfKeys <- fromIntegral <$> getWord32be
    _publicKeysRaw <- getChunk -- not used
    case cipherAlgo of
        "none" -> embeddingDecryption (parsePrivateKeys numberOfKeys) pure
        "aes256-cbc" -> do
            embeddingDecryption (parsePrivateKeys numberOfKeys)
                $ \encrypted ->
                    decryptPrivateKeys deriveKey $ \cipher iv ->
                        Cipher.cbcDecrypt cipher iv encrypted
        "aes256-ctr" -> do
            embeddingDecryption (parsePrivateKeys numberOfKeys)
                $ \encrypted ->
                    decryptPrivateKeys deriveKey $ \cipher iv ->
                        Cipher.ctrCombine cipher iv encrypted
        _ ->
            fail $ "Unsupported cipher algorithm " ++ BC.unpack cipherAlgo

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
                let key = B.take keySize keyIV
                case Cipher.makeIV (B.drop keySize keyIV) of
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
    -> Get [(KeyPair, ByteString)]
parsePrivateKeys count = do
    check1 <- getWord32be
    check2 <- getWord32be
    when (check1 /= check2) (fail "Unsuccessful decryption")
    replicateM count $ do
        key <-
            getChunk >>= \algo -> case algo of
                "ssh-ed25519" -> do
                    skip 3
                    byte 32 -- length field (is always 32 for ssh-ed25519)
                    skip Ed25519.publicKeySize
                    skip 3
                    byte 64 -- length field (is always 64 for ssh-ed25519)
                    secretKeyRaw <- getByteString 32
                    publicKeyRaw <- getByteString 32
                    let key =
                            KeyPairEd25519
                                <$> Ed25519.publicKey publicKeyRaw
                                <*> Ed25519.secretKey secretKeyRaw
                    case key of
                        CryptoPassed a -> pure a
                        CryptoFailed _ ->
                            fail
                                $ "Invalid "
                                    ++ BC.unpack algo
                                    ++ " key"
                _ ->
                    fail
                        $ "Unsupported algorithm "
                            ++ BC.unpack algo
        comment <- getChunk
        pure (key, comment)

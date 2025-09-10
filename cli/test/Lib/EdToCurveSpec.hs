module Lib.EdToCurveSpec (spec) where

import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Curve25519 qualified as Curve25519
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteString qualified as B
import Lib.EdToCurve (ed25519ToCurve25519PublicKey)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "EdToCurve golden" $ do
        it "should convert edward1pk to curve1pk" $ do
            ed25519ToCurve25519PublicKey edward1pk
                `shouldBe` Right curve1pk
        it "should convert edward2p to curve2pk" $ do
            ed25519ToCurve25519PublicKey edward2pk
                `shouldBe` Right curve2pk
        it "should convert edward3pk to curve3pk" $ do
            ed25519ToCurve25519PublicKey edward3pk
                `shouldBe` Right curve3pk
        it "should convert edward4pk to curve4pk" $ do
            ed25519ToCurve25519PublicKey edward4pk
                `shouldBe` Right curve4pk

failCrypto :: CryptoFailable a -> a
failCrypto ((CryptoFailed err)) = error (show err)
failCrypto ((CryptoPassed a)) = a

edward1pk :: Ed25519.PublicKey
edward1pk =
    failCrypto
        $ Ed25519.publicKey
        $ B.pack
            [ 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            ]
curve1pk :: Curve25519.PublicKey
curve1pk =
    failCrypto
        $ Curve25519.publicKey
        $ B.pack
            [ 1
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            ]

edward2pk :: Ed25519.PublicKey
edward2pk =
    failCrypto
        $ Ed25519.publicKey
        $ B.pack
            [ 59
            , 106
            , 39
            , 188
            , 206
            , 182
            , 164
            , 45
            , 98
            , 163
            , 168
            , 208
            , 42
            , 111
            , 13
            , 115
            , 101
            , 50
            , 21
            , 119
            , 29
            , 226
            , 67
            , 166
            , 58
            , 192
            , 72
            , 161
            , 139
            , 89
            , 218
            , 41
            ]
curve2pk :: Curve25519.PublicKey
curve2pk =
    failCrypto
        $ Curve25519.publicKey
        $ B.pack
            [ 91
            , 245
            , 92
            , 115
            , 184
            , 46
            , 190
            , 34
            , 190
            , 128
            , 243
            , 67
            , 6
            , 103
            , 175
            , 87
            , 15
            , 174
            , 37
            , 86
            , 166
            , 65
            , 94
            , 107
            , 48
            , 212
            , 6
            , 83
            , 0
            , 170
            , 148
            , 125
            ]

edward3pk :: Ed25519.PublicKey
edward3pk =
    failCrypto
        $ Ed25519.publicKey
        $ B.pack
            [ 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            ]

curve3pk :: Curve25519.PublicKey
curve3pk =
    failCrypto
        $ Curve25519.publicKey
        $ B.pack
            [ 63
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            ]

edward4pk :: Ed25519.PublicKey
edward4pk =
    failCrypto
        $ Ed25519.publicKey
        $ B.pack
            [ 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            ]
curve4pk :: Curve25519.PublicKey
curve4pk =
    failCrypto
        $ Curve25519.publicKey
        $ B.pack
            [ 104
            , 95
            , 90
            , 83
            , 157
            , 170
            , 246
            , 17
            , 94
            , 54
            , 137
            , 75
            , 37
            , 49
            , 21
            , 237
            , 195
            , 120
            , 99
            , 23
            , 144
            , 27
            , 226
            , 175
            , 12
            , 45
            , 209
            , 61
            , 12
            , 42
            , 81
            , 21
            ]

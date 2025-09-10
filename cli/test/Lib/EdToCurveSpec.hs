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
        it "should convert edward1 to curve1" $ do
            ed25519ToCurve25519PublicKey edward1
                `shouldBe` Right curve1
        it "should convert edward2 to curve2" $ do
            ed25519ToCurve25519PublicKey edward2
                `shouldBe` Right curve2
        it "should convert edward3 to curve3" $ do
            ed25519ToCurve25519PublicKey edward3
                `shouldBe` Right curve3
        it "should convert edward4 to curve4" $ do
            ed25519ToCurve25519PublicKey edward4
                `shouldBe` Right curve4

failCrypto :: CryptoFailable a -> a
failCrypto ((CryptoFailed err)) = error (show err)
failCrypto ((CryptoPassed a)) = a

edward1 :: Ed25519.PublicKey
edward1 =
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
curve1 :: Curve25519.PublicKey
curve1 =
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

edward2 :: Ed25519.PublicKey
edward2 =
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
curve2 :: Curve25519.PublicKey
curve2 =
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

edward3 :: Ed25519.PublicKey
edward3 =
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

curve3 :: Curve25519.PublicKey
curve3 =
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

edward4 :: Ed25519.PublicKey
edward4 =
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
curve4 :: Curve25519.PublicKey
curve4 =
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

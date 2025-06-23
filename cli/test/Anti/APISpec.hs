module Anti.APISpec (spec)
where

import Cardano.Ledger.Alonzo.Tx (AlonzoTx)
import Cardano.Ledger.Api
    ( Addr (..)
    , ConwayEra
    , Datum (..)
    , EraTx (..)
    , EraTxBody (outputsTxBodyL)
    , binaryDataToData
    , eraProtVerLow
    , getPlutusData
    )
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (BabbageTxOut))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Binary
    ( DecCBOR (decCBOR)
    , decodeFullAnnotator
    )
import Cardano.Ledger.Core
    ( SafeToHash (originalBytes)
    , ScriptHash (..)
    )
import Cardano.Ledger.Credential (Credential (..))
import Control.Lens (to, (^.))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Core.Types
    ( Address (..)
    , CageDatum (..)
    , Change (..)
    , Key (..)
    , Operation (..)
    , Owner (..)
    , Platform (..)
    , PublicKeyHash (PublicKeyHash)
    , RequestRefId (RequestRefId)
    , TokenId (..)
    , Username (..)
    , Wallet (..)
    , WithUnsignedTx (WithUnsignedTx)
    )
import Data.Bifunctor (first)
import Data.ByteString.Base16
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Sequence.Strict qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import MPFS.API
    ( RequestDeleteBody (RequestDeleteBody)
    , RequestInsertBody (RequestInsertBody)
    , RequestUpdateBody (RequestUpdateBody)
    , getToken
    , getTokenFacts
    , requestDelete
    , requestInsert
    , requestUpdate
    , waitNBlocks
    )
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import PlutusTx (Data, fromData)
import Servant.Client (ClientM, mkClientEnv, parseBaseUrl, runClientM)
import Submitting (walletFromMnemonic)
import System.Environment (getEnv)
import Test.Hspec (SpecWith, beforeAll, describe, it, shouldBe)
import Text.JSON.Canonical (JSString, JSValue (..), fromJSString)
import User.Cli (UserCommand (..), userCmd)
import User.Requester.Cli (RequesterCommand (..), requesterCmd)
import User.Types (RegisterPublicKey (..))

mpfsPolicyId :: String
mpfsPolicyId = "c1e392ee7da9415f946de9d2aef9607322b47d6e84e8142ef0c340bf"

antiTokenId :: TokenId
antiTokenId =
    TokenId
        "1547b8cb65b187b4b735f00ab4e874084c1947afb11f3a28439fff694adb0e10"

antiTokenOwner :: String
antiTokenOwner = "455dbd55e5cd29ec3239af931eecb30cb295e7f64053ebaae6c496b1"

fundedTestsAddress :: Address
fundedTestsAddress =
    Address
        "addr_test1qz6zuvdm0gu3q54pk50wjfjwyt4mwj6uaelzdfh9extxgn9jwpzyryhlkvscdgpkgefv78gkfa70p70tz04hjpeemjmsrd2jqm"

testUTxOOwner :: Owner
testUTxOOwner = Owner "b42e31bb7a391052a1b51ee9264e22ebb74b5cee7e26a6e5c996644c"

host :: String
host = "https://mpfs.plutimus.com"

newtype Call = Call (forall a. ClientM a -> IO a)

-- | CBOR deserialization of a tx in any era.
deserializeTx :: Text -> AlonzoTx ConwayEra
deserializeTx tx = case decode (T.encodeUtf8 tx) of
    Left err -> error $ "Failed to decode CBOR: " ++ show err
    Right bs -> case decodeFullAnnotator
        (eraProtVerLow @ConwayEra)
        "ConwayTx"
        decCBOR
        $ BL.fromStrict bs of
        Left err -> error $ "Failed to decode full CBOR: " ++ show err
        Right tx' -> tx'

setup :: IO Call
setup = do
    url <- parseBaseUrl host
    nm <- newManager tlsManagerSettings
    let call :: ClientM a -> IO a
        call f = do
            r <- runClientM f (mkClientEnv nm url)
            case r of
                Left err -> error $ "API call failed: " ++ show err
                Right res -> return res
    return $ Call call

getFirstOutput :: AlonzoTx ConwayEra -> Maybe (String, Data)
getFirstOutput dtx = case dtx
    ^. bodyTxL
        . outputsTxBodyL
        . to (Seq.lookup 0) of
    Just
        ( BabbageTxOut
                (Addr Testnet (ScriptHashObj (ScriptHash addr)) _)
                _
                (Datum datum)
                _
            ) ->
            Just
                ( B.unpack $ encode $ originalBytes addr
                , getPlutusData $ binaryDataToData datum
                )
    _ -> error "No outputs found or output is not a BabbageTxOut"

(!?) :: [(JSString, JSValue)] -> String -> Maybe JSValue
(!?) = flip lookup . fmap (first fromJSString)
spec :: SpecWith ()
spec = do
    beforeAll setup $ do
        describe "MPFS.API" $ do
            it "can retrieve config" $ \(Call call) -> do
                res <- call $ getToken antiTokenId
                case res of
                    JSObject obj -> case obj !? "state" of
                        Just (JSObject state) -> case state !? "owner" of
                            Just (JSString antiTokenOwner') ->
                                fromJSString antiTokenOwner' `shouldBe` antiTokenOwner
                            _ -> error "Field 'ewner' is missing or not a string"
                        _ -> error "Field 'state' is missing or not an object"
                    _ -> error "Response is not an object"
            it "can retrieve token facts" $ \(Call call) -> do
                res <- call $ getTokenFacts antiTokenId
                case res of
                    JSObject _obj -> return () -- Add your logic here to handle the object
                    _ -> error "Response is not an object"
            it "can retrieve a request-insert tx" $ \(Call call) -> do
                WithUnsignedTx tx _ <-
                    call
                        $ requestInsert
                            fundedTestsAddress
                            antiTokenId
                        $ RequestInsertBody
                            (JSString "key")
                        $ JSString "value"
                let dtx = deserializeTx tx
                Just (policyId', datum) <- pure $ getFirstOutput dtx
                Just (Just (cageDatum :: CageDatum String String)) <-
                    pure $ fromData datum
                policyId' `shouldBe` mpfsPolicyId
                cageDatum
                    `shouldBe` RequestDatum
                        { tokenId = antiTokenId
                        , owner = testUTxOOwner
                        , change =
                            Change
                                { key = Key "key"
                                , operation = Insert "value"
                                }
                        }
            it "can retrieve a request-delete tx" $ \(Call call) -> do
                WithUnsignedTx tx _ <-
                    call
                        $ requestDelete
                            fundedTestsAddress
                            antiTokenId
                        $ RequestDeleteBody (JSString "key") (JSString "value")
                let dtx = deserializeTx tx
                Just (policyId', datum) <- pure $ getFirstOutput dtx
                Just (Just (cageDatum :: CageDatum String String)) <-
                    pure $ fromData datum
                policyId' `shouldBe` mpfsPolicyId
                cageDatum
                    `shouldBe` RequestDatum
                        { tokenId = antiTokenId
                        , owner = testUTxOOwner
                        , change =
                            Change
                                { key = Key "key"
                                , operation = Delete "value"
                                }
                        }
            it "can retrieve a request-update tx" $ \(Call call) -> do
                WithUnsignedTx tx _ <-
                    call
                        $ requestUpdate
                            fundedTestsAddress
                            antiTokenId
                        $ RequestUpdateBody
                            (JSString "key")
                            (JSString "oldValue")
                            (JSString "newValue")
                let dtx = deserializeTx tx
                Just (policyId', datum) <- pure $ getFirstOutput dtx
                Just (Just (cageDatum :: CageDatum String String)) <-
                    pure $ fromData datum
                policyId' `shouldBe` mpfsPolicyId
                cageDatum
                    `shouldBe` RequestDatum
                        { tokenId = antiTokenId
                        , owner = testUTxOOwner
                        , change =
                            Change
                                { key = Key "key"
                                , operation = Update "oldValue" "newValue"
                                }
                        }
            it "can submit a request-insert tx" $ \(Call call) -> do
                call $ do
                    wallet <- liftIO loadFundedWallet
                    v <-
                        requesterCmd wallet antiTokenId
                            $ RegisterUser
                            $ RegisterPublicKey
                                { platform = Platform "test-platform"
                                , username = Username "test-user"
                                , pubkeyhash = PublicKeyHash "test-pubkeyhash"
                                }
                    let txHash = case v of
                            JSObject obj -> case obj !? "txHash" of
                                Just (JSString requestTxHash) -> fromJSString requestTxHash
                                _ -> error "Key 'txHash' is missing"
                            _ -> error "Response is not an object"
                    _ <- waitNBlocks 1
                    void
                        $ userCmd wallet antiTokenId
                        $ RetractRequest
                            { outputReference =
                                RequestRefId
                                    $ T.pack txHash <> "-0"
                            }

loadFundedWallet :: IO Wallet
loadFundedWallet =
    either (error . show) id
        . walletFromMnemonic
        . T.words
        . T.pack
        <$> getEnv "ANTI_TEST_MNEMONIC"

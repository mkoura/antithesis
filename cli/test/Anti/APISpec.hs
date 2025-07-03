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
import Cli (Command (..), cmd)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Lens (to, (^.))
import Control.Monad (void)
import Control.Monad.Catch (SomeException, catch)
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
    , TxHash (TxHash)
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
    , getTransaction
    , requestDelete
    , requestInsert
    , requestUpdate
    )
import Network.HTTP.Client
    ( ManagerSettings (managerResponseTimeout)
    , newManager
    , responseTimeoutMicro
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Oracle.Cli (OracleCommand (OracleTokenCommand), oracleCmd)
import Oracle.Token.Cli
    ( TokenCommandCore (..)
    , tokenCmdCore
    , updateTokenCmd
    )
import PlutusTx (Data, fromData)
import Servant.Client (ClientM, mkClientEnv, parseBaseUrl, runClientM)
import Submitting (walletFromMnemonic)
import System.Environment (getEnv)
import Test.Hspec
    ( SpecWith
    , beforeAll
    , describe
    , it
    , shouldBe
    , xit
    )
import Text.JSON.Canonical (JSString, JSValue (..), fromJSString)
import User.Requester.Cli (RequesterCommand (..), requesterCmd)
import User.Types (RegisterUserKey (..))

mpfsPolicyId :: String
mpfsPolicyId = "c1e392ee7da9415f946de9d2aef9607322b47d6e84e8142ef0c340bf"

antiTokenId :: TokenId
antiTokenId =
    TokenId
        "865ebcf5e1d6bafcc121030a6e167474a426271d965b78e36d90485adf540575"

antiTokenOwner :: String
antiTokenOwner = "1f5cebecb4cd1cad6108a86014de9d8f23f9d4477bbddb3e1289b224"

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

setup :: IO (Call, JSValue -> ClientM ())
setup = do
    url <- parseBaseUrl host
    nm <-
        newManager
            $ tlsManagerSettings
                { managerResponseTimeout = responseTimeoutMicro $ 90 * 1000000
                }
    let call :: Call
        call = Call $ \f -> do
            r <- runClientM f (mkClientEnv nm url)
            case r of
                Left err -> throwIO err
                Right res -> return res
    return (call, liftIO . waitTx call)

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

txHash :: JSValue -> String
txHash obj = case obj of
    JSObject mapping -> case mapping !? "txHash" of
        Just (JSString requestTxHash) ->
            fromJSString requestTxHash
        _ -> error "Key missing"
    _ -> error "Response is not an object"

waitTx :: Call -> JSValue -> IO ()
waitTx (Call call) obj = void $ go 600
  where
    go :: Int -> IO JSValue
    go 0 = error "Transaction not found after waiting"
    go n =
        do
            call (getTransaction (TxHash $ T.pack $ txHash obj))
            `catch` \(_ :: SomeException) -> do
                liftIO $ threadDelay 1000000
                go (n - 1)

retractTx :: Wallet -> JSValue -> ClientM JSValue
retractTx wallet obj = do
    cmd
        wallet
        (Just antiTokenId)
        $ RetractRequest
            { outputReference =
                RequestRefId
                    $ T.pack (txHash obj) <> "-0"
            }

spec :: SpecWith ()
spec = do
    beforeAll setup $ do
        describe "MPFS.API" $ do
            it "can retrieve config" $ \(Call call, _) -> do
                res <- call $ getToken antiTokenId
                case res of
                    JSObject obj -> case obj !? "state" of
                        Just (JSObject state) -> case state !? "owner" of
                            Just (JSString antiTokenOwner') ->
                                fromJSString antiTokenOwner' `shouldBe` antiTokenOwner
                            _ -> error "Field 'ewner' is missing or not a string"
                        _ -> error "Field 'state' is missing or not an object"
                    _ -> error "Response is not an object"
            it "can retrieve token facts" $ \(Call call, _) -> do
                res <- call $ getTokenFacts antiTokenId
                case res of
                    JSObject _obj -> return () -- Add your logic here to handle the object
                    _ -> error "Response is not an object"
            it "can retrieve a request-insert tx" $ \(Call call, _) -> do
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
            it "can retrieve a request-delete tx" $ \(Call call, _) -> do
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
            it "can retrieve a request-update tx" $ \(Call call, _) -> do
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
            it "can submit a request-insert tx" $ \(Call call, wait) -> do
                wallet <- loadRequesterWallet
                call $ do
                    insert <-
                        requesterCmd wallet antiTokenId
                            $ RegisterUser
                            $ RegisterUserKey
                                { platform = Platform "test-platform"
                                , username = Username "test-user"
                                , pubkeyhash = PublicKeyHash "test-pubkeyhash"
                                }
                    wait insert
                    retractTx wallet insert >>= wait
                    pure ()
            it "can submit a request-delete tx" $ \(Call call, wait) -> do
                wallet <- loadRequesterWallet
                call $ do
                    deleteTx <-
                        requesterCmd wallet antiTokenId
                            $ RegisterUser
                            $ RegisterUserKey
                                { platform = Platform "test-platform"
                                , username = Username "test-user"
                                , pubkeyhash = PublicKeyHash "test-pubkeyhash"
                                }
                    wait deleteTx
                    retractTx wallet deleteTx >>= wait
                    pure ()
            xit "can update the anti token with a registered user" $ \(Call call, wait) -> do
                requester <- loadRequesterWallet
                oracle <- loadOracleWallet
                call $ do
                    insertTx <-
                        requesterCmd requester antiTokenId
                            $ RegisterUser
                            $ RegisterUserKey
                                { platform = Platform "test-platform-2"
                                , username = Username "test-user-2"
                                , pubkeyhash = PublicKeyHash "test-pubkeyhash-2"
                                }
                    wait insertTx
                    updateTx <-
                        oracleCmd oracle (Just antiTokenId)
                            $ OracleTokenCommand
                            $ updateTokenCmd
                                [ RequestRefId
                                    $ T.pack
                                    $ txHash insertTx <> "-0"
                                ]
                    wait updateTx
                    pure ()

loadEnvWallet :: String -> IO Wallet
loadEnvWallet envVar = do
    mnemonic <- getEnv envVar
    pure
        $ either (error . show) id
            . walletFromMnemonic
            . T.words
            . T.pack
        $ mnemonic

loadRequesterWallet :: IO Wallet
loadRequesterWallet =
    loadEnvWallet "ANTI_TEST_REQUESTER_MNEMONIC"

loadOracleWallet :: IO Wallet
loadOracleWallet = do
    w <- loadEnvWallet "ANTI_TEST_ORACLE_MNEMONIC"
    print $ address w
    return w

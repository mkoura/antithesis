module Wallet.CliSpec
    ( spec
    )
where

import Data.Text (Text)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldReturn)
import Wallet.Cli
    ( WalletCommand (Create)
    , WalletError (WalletPresent)
    , walletCmd
    )

spec :: Spec
spec = describe "Wallet create" $ do
    it "fails if the wallet is already present" $ do
        withSystemTempDirectory "wallet-cli-spec" $ \dir -> do
            let wallet = dir <> "/wallet"
            writeFile wallet ""
            let command = Create wallet (Nothing :: Maybe Text)
            walletCmd command `shouldReturn` Left WalletPresent

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Submitting
    ( submitting
    )
where

import Oracle.Token.API (submitTransaction)
import Servant.Client (ClientM)
import Types
    ( Address
    , SignedTx (SignedTx)
    , TxHash (..)
    , Wallet (..)
    , WithTxHash (..)
    , WithUnsignedTx (..)
    )

submitting
    :: Wallet
    -> (Address -> ClientM WithUnsignedTx)
    -> ClientM WithTxHash
submitting Wallet{address, sign} action = do
    WithUnsignedTx unsignedTx v <- action address
    let signedTx = sign unsignedTx
    TxHash txHash <- submitTransaction $ SignedTx signedTx
    return $ WithTxHash{txHash = txHash, value = v}

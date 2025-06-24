module Submitting
    ( submittingFake
    , Submitting
    )
where

import Core.Types
    ( Address
    , Wallet (..)
    , WithTxHash
    , WithUnsignedTx (..)
    )
import Servant.Client (ClientM)

type Submitting =
    Wallet
    -> (Address -> ClientM WithUnsignedTx)
    -> ClientM WithTxHash

submittingFake
    :: Wallet
    -> (Address -> ClientM WithUnsignedTx)
    -> ClientM WithUnsignedTx
submittingFake Wallet{address, sign = _sign} action = do
    r@(WithUnsignedTx _unsignedTx _v) <- action address
    -- let signedTx = sign unsignedTx
    -- TxHash txHash <- submitTransaction $ SignedTx signedTx
    return r

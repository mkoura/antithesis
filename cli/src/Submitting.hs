module Submitting
    ( submittingFake
    , Submitting
    , submitting
    )
where

import Core.Types
    ( Address
    , Wallet (..)
    , WithTxHash
    , WithUnsignedTx (..)
    )
import Data.Aeson (Value)
import Servant.Client (ClientM)

type Submitting =
    Wallet
    -> (Address -> ClientM (WithUnsignedTx Value))
    -> ClientM WithTxHash

submitting :: Submitting
submitting = undefined

submittingFake
    :: Wallet
    -> (Address -> ClientM (WithUnsignedTx Value))
    -> ClientM (WithUnsignedTx Value)
submittingFake Wallet{address, sign = _sign} action = do
    r@(WithUnsignedTx _unsignedTx _v) <- action address
    -- let signedTx = sign unsignedTx
    -- TxHash txHash <- submitTransaction $ SignedTx signedTx
    return r

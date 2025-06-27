{-# LANGUAGE StrictData #-}

module Oracle.Types
    ( Request (..)
    ) where

import Core.Types (Key, Operation, Owner, RequestRefId)

data Request k v = Request
    { ref :: RequestRefId
    , owner :: Owner
    , key :: Key k
    , operation :: Operation v
    }
    deriving (Show, Eq)

{-# LANGUAGE StrictData #-}

module Oracle.Types
    ( Request (..)
    ) where

import Core.Types (Key, Operation, Owner, RequestRefId)

data Request = Request
    { ref :: RequestRefId
    , owner :: Owner
    , key :: Key
    , operation :: Operation
    }
    deriving (Show, Eq)

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Oracle.Token.Cli
    ( tokenCmd
    ) where

import Data.Aeson (ToJSON (..), Value)
import Oracle.Token.API
    ( getToken
    , updateToken
    )

import Servant.Client (ClientM)
import Submitting (submitting)
import Types
    ( TokenCommand (..)
    , TokenId
    , Wallet
    )

tokenCmd :: Wallet -> TokenId -> TokenCommand -> ClientM Value
tokenCmd wallet tk command = do
    case command of
        GetToken -> getToken tk
        UpdateToken reqs -> fmap toJSON
            $ submitting wallet
            $ \address -> updateToken address tk reqs

-- updateToken tk $ RequestRefs reqs

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
import Submitting (submittingFake)
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
            $ submittingFake wallet
            $ \address -> updateToken address tk reqs

-- updateToken tk $ RequestRefs reqs

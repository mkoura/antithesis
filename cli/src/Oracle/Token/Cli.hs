module Oracle.Token.Cli
    ( tokenCmd
    , TokenCommand (..)
    ) where

import Data.Aeson (ToJSON (..), Value)
import MPFS.API
    ( getToken
    , updateToken
    )

import Servant.Client (ClientM)
import Submitting (submittingFake)
import Types
    ( RequestRefId
    , TokenId
    , Wallet
    )

data TokenCommand
    = GetToken
    | UpdateToken
        { requests :: [RequestRefId]
        }
    deriving (Eq, Show)

tokenCmd :: Wallet -> TokenId -> TokenCommand -> ClientM Value
tokenCmd wallet tk command = do
    case command of
        GetToken -> getToken tk
        UpdateToken reqs -> fmap toJSON
            $ submittingFake wallet
            $ \address -> updateToken address tk reqs

-- updateToken tk $ RequestRefs reqs

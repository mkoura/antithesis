module Oracle.Token.Cli
    ( tokenCmd
    , TokenCommand (..)
    ) where

import Core.Types
import MPFS.API
    ( getToken
    , updateToken
    )

import Servant.Client (ClientM)
import Submitting (submittingFake)
import Text.JSON.Canonical (JSValue, ToJSON (..))

data TokenCommand
    = GetToken
    | UpdateToken
        { requests :: [RequestRefId]
        }
    deriving (Eq, Show)

tokenCmd :: Wallet -> TokenId -> TokenCommand -> ClientM JSValue
tokenCmd wallet tk command = do
    case command of
        GetToken -> getToken tk
        UpdateToken reqs -> do
            result <- submittingFake wallet $ \address ->
                updateToken address tk reqs
            toJSON result

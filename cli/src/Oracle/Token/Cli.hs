{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Oracle.Token.Cli
    ( tokenCmd
    ) where

import Data.Aeson (Value)
import Oracle.Token.API
    ( createToken
    , deleteToken
    , getToken
    , updateToken
    )
import Oracle.Types
    ( RequestRefs (..)
    )
import Servant.Client (ClientM)
import Types
    ( TokenCommand (..)
    )

tokenCmd :: TokenCommand -> ClientM Value
tokenCmd command = do
    case command of
        CreateToken -> createToken
        DeleteToken tk -> deleteToken tk
        GetToken tk -> getToken tk
        UpdateToken tk reqs -> updateToken tk $ RequestRefs reqs

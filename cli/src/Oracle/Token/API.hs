{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Oracle.Token.API
    ( tokenApi
    , TokenAPI
    , requestChange
    , retractRequest
    , createToken
    , deleteToken
    , getToken
    , updateToken
    , getTokenFacts
    ) where

import Data.Aeson (Value)
import Data.Data (Proxy (..))
import Oracle.Types (RequestRefs)
import Servant.API
    ( Capture
    , Delete
    , Get
    , JSON
    , Post
    , Put
    , ReqBody
    , (:<|>) (..)
    , type (:>)
    )
import Servant.Client (ClientM, client)
import Types (Request, TokenId)

type TokenAPI =
    "token"
        :> Capture "tokenId" TokenId
        :> "request"
        :> ReqBody '[JSON] Request
        :> Post '[JSON] Value
        :<|> "request"
            :> Capture "txHash" String
            :> Capture "outputIndex" Int
            :> Delete '[JSON] Value
        :<|> "token"
            :> Post '[JSON] Value
        :<|> "token"
            :> Capture "tokenId" TokenId
            :> Delete '[JSON] Value
        :<|> "token"
            :> Capture "tokenId" TokenId
            :> Get '[JSON] Value
        :<|> "token"
            :> Capture "tokenId" TokenId
            :> ReqBody '[JSON] RequestRefs
            :> Put '[JSON] Value
        :<|> "token"
            :> Capture "tokenId" TokenId
            :> "facts"
            :> Get '[JSON] Value

tokenApi :: Proxy TokenAPI
tokenApi = Proxy

createToken :: ClientM Value
deleteToken :: TokenId -> ClientM Value
updateToken :: TokenId -> RequestRefs -> ClientM Value
getToken :: TokenId -> ClientM Value
retractRequest :: [Char] -> Int -> ClientM Value
getTokenFacts :: TokenId -> ClientM Value
requestChange
    :: TokenId
    -> Request
    -> ClientM Value
requestChange
    :<|> retractRequest
    :<|> createToken
    :<|> deleteToken
    :<|> getToken
    :<|> updateToken
    :<|> getTokenFacts =
        client tokenApi

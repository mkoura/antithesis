module Oracle.Token.API
    ( tokenApi
    , TokenAPI
    , requestChange
    , retractChange
    , updateToken
    , getToken
    , getTokenFacts
    , submitTransaction
    ) where

import Data.Aeson (Value)
import Data.Data (Proxy (..))
import Servant.API
    ( Capture
    , Get
    , JSON
    , Post
    , QueryParam'
    , QueryParams
    , ReqBody
    , Required
    , (:<|>) (..)
    , type (:>)
    )
import Servant.Client (ClientM, client)
import Types
    ( Address
    , Operation
    , RequestRefId
    , SignedTx
    , TokenId
    , TxHash
    , WithUnsignedTx
    )

type RequestChange =
    "transaction"
        :> Capture "address" Address
        :> "request-change"
        :> Capture "tokenId" TokenId
        :> QueryParam' '[Required] "key" String
        :> QueryParam' '[Required] "value" String
        :> QueryParam' '[Required] "operation" Operation
        :> Get '[JSON] WithUnsignedTx

type RetractChange =
    "transaction"
        :> Capture "address" Address
        :> "retract-request"
        :> Capture "requestId" RequestRefId
        :> Get '[JSON] WithUnsignedTx

type UpdateToken =
    "transaction"
        :> Capture "address" Address
        :> Capture "tokenId" TokenId
        :> QueryParams "request" RequestRefId
        :> Get '[JSON] WithUnsignedTx

type GetToken =
    "token"
        :> Capture "tokenId" TokenId
        :> Get '[JSON] Value

type GetTokenFacts =
    "token"
        :> Capture "tokenId" TokenId
        :> "facts"
        :> Get '[JSON] Value

type SubmitTransaction =
    "transaction"
        :> ReqBody '[JSON] SignedTx
        :> Post '[JSON] TxHash

type TokenAPI =
    RequestChange
        :<|> RetractChange
        :<|> UpdateToken
        :<|> GetToken
        :<|> GetTokenFacts
        :<|> SubmitTransaction

tokenApi :: Proxy TokenAPI
tokenApi = Proxy

requestChange
    :: Address
    -> TokenId
    -> String
    -> String
    -> Operation
    -> ClientM WithUnsignedTx
retractChange :: Address -> RequestRefId -> ClientM WithUnsignedTx
updateToken
    :: Address -> TokenId -> [RequestRefId] -> ClientM WithUnsignedTx
getToken :: TokenId -> ClientM Value
getTokenFacts :: TokenId -> ClientM Value
submitTransaction :: SignedTx -> ClientM TxHash
requestChange
    :<|> retractChange
    :<|> updateToken
    :<|> getToken
    :<|> getTokenFacts
    :<|> submitTransaction =
        client tokenApi

module Oracle.Token.API
    ( tokenApi
    , TokenAPI
    , requestInsert
    , requestDelete
    , requestUpdate
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
    , RequestRefId
    , SignedTx
    , TokenId
    , TxHash
    , WithUnsignedTx
    )

type RequestInsert =
    "transaction"
        :> Capture "address" Address
        :> "request-insert"
        :> Capture "tokenId" TokenId
        :> QueryParam' '[Required] "key" String
        :> QueryParam' '[Required] "value" String
        :> Get '[JSON] WithUnsignedTx

type RequestDelete =
    "transaction"
        :> Capture "address" Address
        :> "request-delete"
        :> Capture "tokenId" TokenId
        :> QueryParam' '[Required] "key" String
        :> QueryParam' '[Required] "value" String
        :> Get '[JSON] WithUnsignedTx

type RequestUpdate =
    "transaction"
        :> Capture "address" Address
        :> "request-update"
        :> Capture "tokenId" TokenId
        :> QueryParam' '[Required] "key" String
        :> QueryParam' '[Required] "oldValue" String
        :> QueryParam' '[Required] "newValue" String
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
    RequestInsert
        :<|> RequestDelete
        :<|> RequestUpdate
        :<|> RetractChange
        :<|> UpdateToken
        :<|> GetToken
        :<|> GetTokenFacts
        :<|> SubmitTransaction

tokenApi :: Proxy TokenAPI
tokenApi = Proxy

requestInsert
    :: Address
    -> TokenId
    -> String
    -> String
    -> ClientM WithUnsignedTx
requestDelete
    :: Address
    -> TokenId
    -> String
    -> String
    -> ClientM WithUnsignedTx
requestUpdate
    :: Address
    -> TokenId
    -> String
    -> String
    -> String
    -> ClientM WithUnsignedTx
retractChange :: Address -> RequestRefId -> ClientM WithUnsignedTx
updateToken
    :: Address -> TokenId -> [RequestRefId] -> ClientM WithUnsignedTx
getToken :: TokenId -> ClientM Value
getTokenFacts :: TokenId -> ClientM Value
submitTransaction :: SignedTx -> ClientM TxHash
requestInsert
    :<|> requestDelete
    :<|> requestUpdate
    :<|> retractChange
    :<|> updateToken
    :<|> getToken
    :<|> getTokenFacts
    :<|> submitTransaction =
        client tokenApi

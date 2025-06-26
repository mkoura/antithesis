module MPFS.API
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
    , waitNBlocks
    , RequestInsertBody (..)
    , RequestDeleteBody (..)
    , RequestUpdateBody (..)
    ) where

import Core.Types
    ( Address
    , RequestRefId
    , SignedTx
    , TokenId
    , TxHash
    , WithUnsignedTx
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Data (Proxy (..))
import Lib.JSON (fromAesonThrow)
import Servant.API
    ( Capture
    , Get
    , JSON
    , Post
    , QueryParams
    , ReqBody
    , (:<|>) (..)
    , type (:>)
    )
import Servant.Client (ClientM, client)
import Text.JSON.Canonical (JSValue)

data RequestInsertBody = RequestInsertBody
    { key :: String
    , value :: String
    }

instance ToJSON RequestInsertBody where
    toJSON (RequestInsertBody k v) = object ["key" .= k, "value" .= v]

instance FromJSON RequestInsertBody where
    parseJSON = withObject "RequestInsertBody" $ \o ->
        RequestInsertBody <$> o .: "key" <*> o .: "value"

data RequestDeleteBody = RequestDeleteBody
    { key :: String
    , value :: String
    }

instance ToJSON RequestDeleteBody where
    toJSON (RequestDeleteBody k v) = object ["key" .= k, "value" .= v]

instance FromJSON RequestDeleteBody where
    parseJSON = withObject "RequestDeleteBody" $ \o ->
        RequestDeleteBody <$> o .: "key" <*> o .: "value"

data RequestUpdateBody = RequestUpdateBody
    { key :: String
    , oldValue :: String
    , newValue :: String
    }

instance ToJSON RequestUpdateBody where
    toJSON (RequestUpdateBody k old new) =
        object ["key" .= k, "oldValue" .= old, "newValue" .= new]

instance FromJSON RequestUpdateBody where
    parseJSON = withObject "RequestUpdateBody" $ \o ->
        RequestUpdateBody
            <$> o .: "key"
            <*> o .: "oldValue"
            <*> o .: "newValue"

type RequestInsert =
    "transaction"
        :> Capture "address" Address
        :> "request-insert"
        :> Capture "tokenId" TokenId
        :> ReqBody '[JSON] RequestInsertBody
        :> Post '[JSON] (WithUnsignedTx Value)

type RequestDelete =
    "transaction"
        :> Capture "address" Address
        :> "request-delete"
        :> Capture "tokenId" TokenId
        :> ReqBody '[JSON] RequestDeleteBody
        :> Post '[JSON] (WithUnsignedTx Value)

type RequestUpdate =
    "transaction"
        :> Capture "address" Address
        :> "request-update"
        :> Capture "tokenId" TokenId
        :> ReqBody '[JSON] RequestUpdateBody
        :> Post '[JSON] (WithUnsignedTx Value)

type RetractChange =
    "transaction"
        :> Capture "address" Address
        :> "retract-request"
        :> Capture "requestId" RequestRefId
        :> Get '[JSON] (WithUnsignedTx Value)

type UpdateToken =
    "transaction"
        :> Capture "address" Address
        :> Capture "tokenId" TokenId
        :> QueryParams "request" RequestRefId
        :> Get '[JSON] (WithUnsignedTx Value)

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

type WaitNBlocks =
    "wait-n-blocks"
        :> Capture "n" Int
        :> Get '[JSON] Value

type TokenAPI =
    RequestInsert
        :<|> RequestDelete
        :<|> RequestUpdate
        :<|> RetractChange
        :<|> UpdateToken
        :<|> GetToken
        :<|> GetTokenFacts
        :<|> SubmitTransaction
        :<|> WaitNBlocks

tokenApi :: Proxy TokenAPI
tokenApi = Proxy

requestInsert
    :: Address
    -> TokenId
    -> RequestInsertBody
    -> ClientM (WithUnsignedTx JSValue)
requestInsert address tokenId body =
    fmap fromAesonThrow <$> requestInsert' address tokenId body
requestDelete
    :: Address
    -> TokenId
    -> RequestDeleteBody
    -> ClientM (WithUnsignedTx JSValue)
requestDelete address tokenId body =
    fmap fromAesonThrow <$> requestDelete' address tokenId body
requestUpdate
    :: Address
    -> TokenId
    -> RequestUpdateBody
    -> ClientM (WithUnsignedTx JSValue)
requestUpdate address tokenId body =
    fmap fromAesonThrow <$> requestUpdate' address tokenId body
retractChange
    :: Address -> RequestRefId -> ClientM (WithUnsignedTx JSValue)
retractChange address requestId =
    fmap fromAesonThrow <$> retractChange' address requestId
updateToken
    :: Address
    -> TokenId
    -> [RequestRefId]
    -> ClientM (WithUnsignedTx JSValue)
updateToken address tokenId requests =
    fmap fromAesonThrow <$> updateToken' address tokenId requests
getToken :: TokenId -> ClientM JSValue
getToken tokenId = fromAesonThrow <$> getToken' tokenId
getTokenFacts :: TokenId -> ClientM JSValue
getTokenFacts tokenId = fromAesonThrow <$> getTokenFacts' tokenId
submitTransaction :: SignedTx -> ClientM TxHash
submitTransaction = submitTransaction'
waitNBlocks :: Int -> ClientM JSValue
waitNBlocks n = fromAesonThrow <$> waitNBlocks' n

requestInsert'
    :: Address
    -> TokenId
    -> RequestInsertBody
    -> ClientM (WithUnsignedTx Value)
requestDelete'
    :: Address
    -> TokenId
    -> RequestDeleteBody
    -> ClientM (WithUnsignedTx Value)
requestUpdate'
    :: Address
    -> TokenId
    -> RequestUpdateBody
    -> ClientM (WithUnsignedTx Value)
retractChange'
    :: Address -> RequestRefId -> ClientM (WithUnsignedTx Value)
updateToken'
    :: Address -> TokenId -> [RequestRefId] -> ClientM (WithUnsignedTx Value)
getToken' :: TokenId -> ClientM Value
getTokenFacts' :: TokenId -> ClientM Value
submitTransaction' :: SignedTx -> ClientM TxHash
waitNBlocks' :: Int -> ClientM Value
requestInsert'
    :<|> requestDelete'
    :<|> requestUpdate'
    :<|> retractChange'
    :<|> updateToken'
    :<|> getToken'
    :<|> getTokenFacts'
    :<|> submitTransaction'
    :<|> waitNBlocks' =
        client tokenApi

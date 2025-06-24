module Anti.Server
    ( appDummy
    , dummyWithUnsignedTx
    )
where

import Data.Aeson (ToJSON (..), Value)
import MPFS.API (tokenApi)
import Network.Wai (Application)
import Servant (serve, (:<|>) (..))
import Servant.Server (Handler)
import Types
    ( Address
    , RequestRefId
    , SignedTx
    , TokenId
    , TxHash (..)
    , WithUnsignedTx (..)
    )

appDummy :: Application
appDummy =
    serve
        tokenApi
        ( requestInsert
            :<|> requestDelete
            :<|> requestUpdate
            :<|> retractChange
            :<|> updateToken
            :<|> getToken
            :<|> facts
            :<|> submitTransaction
        )

dummyWithUnsignedTx :: WithUnsignedTx
dummyWithUnsignedTx =
    WithUnsignedTx
        { unsignedTransaction = "dummyUnsignedTransaction"
        , value = Nothing
        }

requestInsert
    :: Address
    -> TokenId
    -> String
    -> String
    -> Handler WithUnsignedTx
requestInsert _ _ _ _ = return dummyWithUnsignedTx
requestDelete
    :: Address
    -> TokenId
    -> String
    -> String
    -> Handler WithUnsignedTx
requestDelete _ _ _ _ = return dummyWithUnsignedTx
requestUpdate
    :: Address
    -> TokenId
    -> String
    -> String
    -> String
    -> Handler WithUnsignedTx
requestUpdate _ _ _ _ _ = return dummyWithUnsignedTx

retractChange :: Address -> RequestRefId -> Handler WithUnsignedTx
retractChange _ _ = return dummyWithUnsignedTx

updateToken
    :: Address -> TokenId -> [RequestRefId] -> Handler WithUnsignedTx
updateToken _ _ _ = return dummyWithUnsignedTx

getToken :: TokenId -> Handler Value
getToken _ = pure $ toJSON ()

facts :: TokenId -> Handler Value
facts _ = pure $ toJSON ()

submitTransaction
    :: SignedTx -> Handler TxHash
submitTransaction _ = do
    return $ TxHash "dummyTxHash"

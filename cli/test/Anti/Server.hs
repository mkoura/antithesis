module Anti.Server
    ( appDummy
    , dummyWithUnsignedTx
    )
where

import Core.Types
    ( Address
    , RequestRefId
    , SignedTx
    , TokenId
    , TxHash (..)
    , WithUnsignedTx (..)
    )
import Data.Aeson (ToJSON (..), Value)
import MPFS.API
    ( RequestDeleteBody
    , RequestInsertBody
    , RequestUpdateBody
    , tokenApi
    )
import Network.Wai (Application)
import Servant (serve, (:<|>) (..))
import Servant.Server (Handler)

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
    -> RequestInsertBody
    -> Handler WithUnsignedTx
requestInsert _ _ _ = return dummyWithUnsignedTx
requestDelete
    :: Address
    -> TokenId
    -> RequestDeleteBody
    -> Handler WithUnsignedTx
requestDelete _ _ _ = return dummyWithUnsignedTx
requestUpdate
    :: Address
    -> TokenId
    -> RequestUpdateBody
    -> Handler WithUnsignedTx
requestUpdate _ _ _ = return dummyWithUnsignedTx

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

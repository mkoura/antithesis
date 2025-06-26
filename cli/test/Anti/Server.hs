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
            :<|> const (pure $ toJSON ())
        )

dummyWithUnsignedTx :: WithUnsignedTx Value
dummyWithUnsignedTx =
    WithUnsignedTx
        { unsignedTransaction = "dummyUnsignedTransaction"
        , value = Nothing
        }

requestInsert
    :: Address
    -> TokenId
    -> RequestInsertBody
    -> Handler (WithUnsignedTx Value)
requestInsert _ _ _ = return dummyWithUnsignedTx
requestDelete
    :: Address
    -> TokenId
    -> RequestDeleteBody
    -> Handler (WithUnsignedTx Value)
requestDelete _ _ _ = return dummyWithUnsignedTx
requestUpdate
    :: Address
    -> TokenId
    -> RequestUpdateBody
    -> Handler (WithUnsignedTx Value)
requestUpdate _ _ _ = return dummyWithUnsignedTx

retractChange
    :: Address -> RequestRefId -> Handler (WithUnsignedTx Value)
retractChange _ _ = return dummyWithUnsignedTx

updateToken
    :: Address -> TokenId -> [RequestRefId] -> Handler (WithUnsignedTx Value)
updateToken _ _ _ = return dummyWithUnsignedTx

getToken :: TokenId -> Handler Value
getToken _ = pure $ toJSON ()

facts :: TokenId -> Handler Value
facts _ = pure $ toJSON ()

submitTransaction
    :: SignedTx -> Handler TxHash
submitTransaction _ = do
    return $ TxHash "dummyTxHash"

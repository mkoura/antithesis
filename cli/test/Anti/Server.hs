module Anti.Server
    ( appDummy
    , dummyWithUnsignedTx
    )
where

import Data.Aeson (ToJSON (..), Value)
import Network.Wai (Application)
import Oracle.Token.API (tokenApi)
import Servant (serve, (:<|>) (..))
import Servant.Server (Handler)
import Types
    ( Address,
      Operation,
      TokenId,
      WithUnsignedTx(..),
      RequestRefId,
      TxHash(..),
      SignedTx )

appDummy :: Application
appDummy =
    serve
        tokenApi
        ( requestChange
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

requestChange
    :: Address
    -> TokenId
    -> String
    -> String
    -> Operation
    -> Handler WithUnsignedTx
requestChange _ _ _ _ _ = return dummyWithUnsignedTx

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

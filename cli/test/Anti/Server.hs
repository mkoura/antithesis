{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Anti.Server
    ( appDummy
    , dummyTxId
    )
where

import Oracle.Token.API (tokenApi)
import Oracle.Types (RequestRefs)
import Types (Request (..), TokenId)
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Network.Wai (Application)
import Servant (serve, (:<|>) (..))
import Servant.Server (Handler)

appDummy :: Application
appDummy =
    serve
        tokenApi
        ( postRequestDummy
            :<|> retractRequestDummy
            :<|> postTokenDummy
            :<|> deleteTokenDummy
            :<|> getTokenDummy
            :<|> updateTokenDummy
            :<|> facts
        )

facts :: TokenId -> Handler Value
facts _ = return $ toJSON ([] :: [()])

retractRequestDummy :: [Char] -> Int -> Handler Value
retractRequestDummy _txHash _outputIndex = do
    return dummyTxId

updateTokenDummy :: TokenId -> RequestRefs -> Handler Value
updateTokenDummy _ _ = do
    return dummyTxId

deleteTokenDummy :: TokenId -> Handler Value
deleteTokenDummy _ = return dummyTxId

postTokenDummy :: Handler Value
postTokenDummy = return dummyTokenId

getTokenDummy :: TokenId -> Handler Value
getTokenDummy _ = return dummyTokenId

dummyTokenId :: Value
dummyTokenId = object ["tokenId" .= ("dummyTokenId" :: String)]

dummyTxId :: Value
dummyTxId = object ["txId " .= ("dummyTxId" :: String)]

postRequestDummy :: TokenId -> Request -> Handler Value
postRequestDummy _tokenId = \case
    Request
        { key = _
        , value = _
        , operation = _
        } -> do return dummyTxId

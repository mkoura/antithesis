module Oracle.Token.Cli
    ( tokenCmd
    , TokenCommand (..)
    ) where

import Core.Types
import MPFS.API
    ( bootToken
    , getToken
    , updateToken
    )

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Servant.Client (ClientM)
import Submitting (submitting)
import System.Environment (setEnv)
import Text.JSON.Canonical (JSValue (..), ToJSON (..), fromJSString)

data TokenCommand
    = GetToken
    | BootToken
    | UpdateToken
        { requests :: [RequestRefId]
        }
    deriving (Eq, Show)

tokenCmd :: Wallet -> Maybe TokenId -> TokenCommand -> ClientM JSValue
tokenCmd wallet (Just tk) command = do
    case command of
        GetToken -> getToken tk
        UpdateToken reqs -> do
            result <- submitting wallet $ \address ->
                updateToken address tk reqs
            toJSON result
        BootToken -> error "BootToken command requires no TokenId"
tokenCmd wallet Nothing command = case command of
    BootToken -> do
        result <- submitting wallet $ \address -> do
            w@(WithUnsignedTx{value}) <- bootToken address
            case value of
                Just (JSString v) ->
                    void
                        $ liftIO
                        $ setEnv "ANTI_TOKEN_ID"
                        $ fromJSString v
                _ -> error "BootToken failed, no value returned"
            pure w

        toJSON result
    _ -> error "TokenId is required for this command"

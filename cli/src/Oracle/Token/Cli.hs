module Oracle.Token.Cli
    ( tokenCmdCore
    , TokenCommandCore (..)
    , getTokenCmd
    , bootTokenCmd
    , updateTokenCmd
    , tokenCmd
    , TokenCommand
    ) where

import Core.Types
import Lib.JSON ()
import MPFS.API
    ( bootToken
    , getToken
    , updateToken
    )
import Servant.Client (ClientM)
import Submitting (submitting)
import Text.JSON.Canonical
    ( FromJSON (fromJSON)
    , JSValue (..)
    , ToJSON (..)
    )

getTokenCmd :: TokenCommand
getTokenCmd = AnyTokenCommand GetToken

bootTokenCmd :: TokenCommand
bootTokenCmd = AnyTokenCommand BootToken

updateTokenCmd :: [RequestRefId] -> TokenCommand
updateTokenCmd reqs = AnyTokenCommand $ UpdateToken reqs

tokenCmd
    :: Wallet -> Maybe TokenId -> TokenCommand -> ClientM JSValue
tokenCmd wallet tokenId (AnyTokenCommand cmd) =
    case cmd of
        GetToken -> tokenCmdCore wallet tokenId GetToken
        BootToken -> do
            result <- tokenCmdCore wallet Nothing BootToken
            toJSON result
        UpdateToken reqs -> tokenCmdCore wallet tokenId (UpdateToken reqs)

data TokenCommand = forall a. AnyTokenCommand (TokenCommandCore a)

deriving instance Show TokenCommand
instance Eq TokenCommand where
    AnyTokenCommand GetToken == AnyTokenCommand GetToken = True
    AnyTokenCommand BootToken == AnyTokenCommand BootToken = True
    AnyTokenCommand (UpdateToken reqs1) == AnyTokenCommand (UpdateToken reqs2) =
        reqs1 == reqs2
    _ == _ = False
data TokenCommandCore a where
    GetToken :: TokenCommandCore JSValue
    BootToken :: TokenCommandCore TokenId
    UpdateToken
        :: { requests :: [RequestRefId]
           }
        -> TokenCommandCore JSValue

deriving instance Show (TokenCommandCore a)
deriving instance Eq (TokenCommandCore a)

tokenCmdCore
    :: Wallet -> Maybe TokenId -> TokenCommandCore a -> ClientM a
tokenCmdCore wallet (Just tk) cmd = do
    case cmd of
        GetToken -> getToken tk
        UpdateToken reqs -> do
            result <- submitting wallet $ \address ->
                updateToken address tk reqs
            toJSON result
        BootToken -> error "BootToken command requires no TokenId"
tokenCmdCore wallet Nothing cmd = case cmd of
    BootToken -> do
        WithTxHash _ jTokenId <- submitting wallet
            $ \address -> bootToken address
        case jTokenId of
            Just tkId -> case fromJSON tkId of
                Nothing -> error "BootToken failed, TokenId is not valid JSON"
                Just tokenId -> pure tokenId
            _ -> error "BootToken failed, no TokenId returned"
    _ -> error "TokenId is required for this command"

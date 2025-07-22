module Oracle.Token.Cli
    ( tokenCmdCore
    , TokenCommand (..)
    ) where

import Core.Types.Basic (Owner, RequestRefId, TokenId)
import Core.Types.Tx (TxHash, WithTxHash (WithTxHash))
import Core.Types.Wallet (Wallet)
import Data.Foldable (find, for_)
import Lib.JSON ()
import MPFS.API
    ( bootToken
    , endToken
    , getToken
    , updateToken
    )
import Oracle.Types (Token (..), requestId)
import Oracle.Validate.Request
    ( validateRequest
    )
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig
    )
import Servant.Client (ClientM)
import Submitting (Submitting, signAndSubmit)
import Text.JSON.Canonical
    ( FromJSON (fromJSON)
    , JSValue (..)
    )
import Validation (mkValidation)

data TokenCommand a where
    GetToken :: TokenCommand JSValue
    BootToken :: TokenCommand (WithTxHash TokenId)
    UpdateToken
        :: {requests :: [RequestRefId]} -> TokenCommand TxHash
    EndToken :: TokenCommand TxHash

deriving instance Show (TokenCommand a)
deriving instance Eq (TokenCommand a)

tokenCmdCore
    :: Submitting
    -> Wallet
    -> Maybe TokenId
    -> TestRunValidationConfig
    -> Owner
    -> TokenCommand a
    -> ClientM a
tokenCmdCore sbmt wallet (Just tk) testRunConfig pkh = \case
    GetToken -> getToken tk
    UpdateToken reqs -> do
        WithTxHash txHash _ <- signAndSubmit sbmt wallet $ \address -> do
            mpendings <- fromJSON <$> getToken tk
            case mpendings of
                Nothing -> error "UpdateToken failed, TokenId is not valid JSON"
                Just (token :: Token) -> do
                    for_ reqs $ \reqId -> do
                        (_rq, validationResult) <- case find
                            (\r -> requestId r == reqId)
                            (tokenRequests token) of
                            Nothing ->
                                error
                                    $ "RequestRefId "
                                        ++ show reqId
                                        ++ " not found in Token"
                            Just r ->
                                validateRequest
                                    testRunConfig
                                    pkh
                                    (mkValidation tk)
                                    r
                        if validationResult == Right ()
                            then pure ()
                            else
                                error
                                    $ "RequestRefId "
                                        ++ show reqId
                                        ++ " validation failed: "
                                        ++ show validationResult
                    updateToken address tk reqs
        pure txHash
    BootToken -> error "BootToken command requires no TokenId"
    EndToken -> do
        WithTxHash txHash _ <- signAndSubmit sbmt wallet
            $ \address -> endToken address tk
        pure txHash
tokenCmdCore sbmt wallet Nothing _ _ = \case
    BootToken -> do
        WithTxHash txHash jTokenId <- signAndSubmit sbmt wallet
            $ \address -> bootToken address
        case jTokenId of
            Just tkId -> case fromJSON tkId of
                Nothing -> error "BootToken failed, TokenId is not valid JSON"
                Just tokenId -> pure $ WithTxHash txHash (Just tokenId)
            _ -> error "BootToken failed, no TokenId returned"
    _ -> error "TokenId is required for this command"

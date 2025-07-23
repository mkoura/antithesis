module Oracle.Token.Cli
    ( tokenCmdCore
    , TokenCommand (..)
    ) where

import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Core.Context
    ( WithContext
    , askMkValidation
    , askMpfs
    , askSubmit
    , askTestRunConfig
    , askWalletOwner
    , withMPFS
    )
import Core.Types.Basic (RequestRefId, TokenId)
import Core.Types.Tx (TxHash, WithTxHash (WithTxHash))
import Lib.JSON (object, (.=))
import MPFS.API
    ( MPFS (..)
    , mpfsGetToken
    )
import Oracle.Types
    ( RequestValidationFailure
    , RequestZoo
    , Token (..)
    , requestZooRefId
    )
import Oracle.Validate.Request
    ( validateRequest
    )
import Oracle.Validate.Types
    ( AValidationResult (..)
    , Validate
    , mapFailure
    , notValidated
    , runValidate
    , sequenceValidate
    )
import Submitting (Submission (..))
import Text.JSON.Canonical
    ( FromJSON (fromJSON)
    , JSValue (..)
    , ToJSON (..)
    )

data TokenUpdateRequestValidation = TokenUpdateRequestValidation
    { validationRequestId :: RequestRefId
    , problem :: TokenUpdateRequestValidationProblem
    }
    deriving (Show, Eq)

instance Monad m => ToJSON m TokenUpdateRequestValidation where
    toJSON (TokenUpdateRequestValidation reqId problem) =
        object
            [ "requestId" .= reqId
            , "problem" .= problem
            ]

data TokenUpdateRequestValidationProblem
    = TokenUpdateRequestNotFound
    | TokenUpdateRequestValidationFailure
        RequestValidationFailure
    deriving (Show, Eq)

instance Monad m => ToJSON m TokenUpdateRequestValidationProblem where
    toJSON = \case
        TokenUpdateRequestNotFound ->
            toJSON ("requestNotFound" :: String)
        TokenUpdateRequestValidationFailure e ->
            object ["validationFailure" .= e]

data TokenUpdateFailure
    = TokenNotParsable TokenId
    | TokenUpdateRequestValidations
        [TokenUpdateRequestValidation]
    deriving (Show, Eq)

instance Exception TokenUpdateFailure

instance Monad m => ToJSON m TokenUpdateFailure where
    toJSON = \case
        TokenNotParsable tk ->
            object ["tokenNotParsable" .= tk]
        TokenUpdateRequestValidations validations ->
            object ["tokenUpdateRequestValidations" .= validations]

data TokenCommand a where
    GetToken :: TokenCommand JSValue
    BootToken :: TokenCommand (WithTxHash TokenId)
    UpdateToken
        :: {requests :: [RequestRefId]}
        -> TokenCommand
            ( AValidationResult
                TokenUpdateFailure
                TxHash
            )
    EndToken :: TokenCommand TxHash

deriving instance Show (TokenCommand a)
deriving instance Eq (TokenCommand a)

promoteFailure
    :: Functor m
    => RequestZoo
    -> Validate RequestValidationFailure m a
    -> Validate TokenUpdateRequestValidation m a
promoteFailure req =
    mapFailure
        ( TokenUpdateRequestValidation
            (requestZooRefId req)
            . TokenUpdateRequestValidationFailure
        )

tokenCmdCore
    :: Monad m
    => Maybe TokenId
    -> TokenCommand a
    -> WithContext m a
tokenCmdCore (Just tk) = \case
    GetToken -> withMPFS $ \mpfs -> mpfsGetToken mpfs tk
    UpdateToken reqs -> do
        mpfs <- askMpfs
        testRunConfig <- askTestRunConfig
        pkh <- askWalletOwner
        validation <- askMkValidation tk
        Submission submit <- askSubmit
        lift $ runValidate $ do
            mpendings <- lift $ fromJSON <$> mpfsGetToken mpfs tk
            case mpendings of
                Nothing -> notValidated $ TokenNotParsable tk
                Just (token :: Token) ->
                    void
                        $ mapFailure TokenUpdateRequestValidations
                        $ sequenceValidate
                        $ promoteFailure
                            <*> validateRequest testRunConfig pkh validation
                            <$> tokenRequests token
            WithTxHash txHash _ <- lift
                $ submit
                $ \address -> mpfsUpdateToken mpfs address tk reqs
            pure txHash
    BootToken -> error "BootToken command requires no TokenId"
    EndToken -> do
        mpfs <- askMpfs
        Submission submit <- askSubmit
        WithTxHash txHash _ <- lift
            $ submit
            $ \address -> mpfsEndToken mpfs address tk
        pure txHash
tokenCmdCore Nothing = \case
    BootToken -> do
        mpfs <- askMpfs
        Submission submit <- askSubmit
        WithTxHash txHash jTokenId <- lift
            $ submit
            $ \address -> mpfsBootToken mpfs address
        case jTokenId of
            Just tkId -> case fromJSON tkId of
                Nothing -> error "BootToken failed, TokenId is not valid JSON"
                Just tokenId -> pure $ WithTxHash txHash (Just tokenId)
            _ -> error "BootToken failed, no TokenId returned"
    _ -> error "TokenId is required for this command"

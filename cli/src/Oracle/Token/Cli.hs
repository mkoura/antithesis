module Oracle.Token.Cli
    ( tokenCmdCore
    , TokenCommand (..)
    ) where

import Control.Exception (Exception)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Core.Context
    ( WithContext
    , askConfig
    , askMpfs
    , askSubmit
    , askValidation
    )
import Core.Types.Basic (RequestRefId, TokenId)
import Core.Types.Tx (TxHash, WithTxHash (WithTxHash))
import Data.Functor ((<&>))
import Data.List (find)
import Lib.JSON.Canonical.Extra (object, (.=))
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
    , liftMaybe
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
            toJSON ("Request not found" :: String)
        TokenUpdateRequestValidationFailure e ->
            object ["validationFailure" .= e]

data TokenUpdateFailure
    = TokenNotParsable TokenId
    | TokenUpdateOfNoRequests
    | TokenUpdateRequestValidations
        [TokenUpdateRequestValidation]
    | TokenUpdateConfigNotFound
    deriving (Show, Eq)

instance Exception TokenUpdateFailure

instance Monad m => ToJSON m TokenUpdateFailure where
    toJSON = \case
        TokenNotParsable tk ->
            object ["tokenNotParsable" .= tk]
        TokenUpdateOfNoRequests ->
            toJSON ("No requests to include" :: String)
        TokenUpdateRequestValidations validations ->
            object ["tokenUpdateRequestValidations" .= validations]
        TokenUpdateConfigNotFound ->
            toJSON ("Token update config not available" :: String)

data TokenCommand a where
    GetToken :: TokenId -> TokenCommand JSValue
    BootToken :: TokenCommand (WithTxHash TokenId)
    UpdateToken
        :: TokenId
        -> [RequestRefId]
        -> TokenCommand
            ( AValidationResult
                TokenUpdateFailure
                TxHash
            )
    EndToken :: TokenId -> TokenCommand TxHash

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
    :: (MonadIO m)
    => TokenCommand a
    -> WithContext m a
tokenCmdCore command = do
    mpfs <- askMpfs
    Submission submit <- askSubmit
    case command of
        GetToken tk -> lift $ mpfsGetToken mpfs tk
        UpdateToken tk wanted -> do
            validation <- askValidation tk
            mconfig <- askConfig tk
            lift $ runValidate $ do
                config <- liftMaybe TokenUpdateConfigNotFound mconfig
                when (null wanted) $ notValidated TokenUpdateOfNoRequests
                mpendings <- lift $ fromJSON <$> mpfsGetToken mpfs tk
                token <- liftMaybe (TokenNotParsable tk) mpendings
                let requests = tokenRequests token
                when (null requests) $ notValidated TokenUpdateOfNoRequests
                void
                    $ mapFailure TokenUpdateRequestValidations
                    $ sequenceValidate
                    $ wanted
                    <&> \req -> do
                        tokenRequest <-
                            liftMaybe
                                (TokenUpdateRequestValidation req TokenUpdateRequestNotFound)
                                $ find ((== req) . requestZooRefId) requests
                        promoteFailure tokenRequest
                            $ validateRequest config validation tokenRequest
                WithTxHash txHash _ <- lift
                    $ submit
                    $ \address -> mpfsUpdateToken mpfs address tk wanted
                pure txHash
        BootToken -> lift $ do
            WithTxHash txHash jTokenId <- submit
                $ \address -> mpfsBootToken mpfs address
            case jTokenId of
                Just tkId -> case fromJSON tkId of
                    Nothing -> error "BootToken failed, TokenId is not valid JSON"
                    Just tokenId -> pure $ WithTxHash txHash (Just tokenId)
                _ -> error "BootToken failed, no TokenId returned"
        EndToken tk -> lift $ do
            WithTxHash txHash _ <- submit
                $ \address -> mpfsEndToken mpfs address tk
            pure txHash

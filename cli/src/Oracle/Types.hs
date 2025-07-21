{-# LANGUAGE StrictData #-}

module Oracle.Types
    ( Request (..)
    , Token (..)
    , TokenState (..)
    , RequestZoo (..)
    , requestId
    ) where

import Control.Applicative (Alternative, (<|>))
import Core.Types.Basic (Owner, RequestRefId)
import Core.Types.Change (Change (..))
import Core.Types.Operation (Op (..), Operation (..))
import Core.Types.Tx (Root)
import Lib.JSON (object, withObject, (.:), (.=))
import Text.JSON.Canonical
    ( FromJSON (..)
    , ReportSchemaErrors
    , ToJSON (..)
    )
import User.Types
    ( Phase (..)
    , RegisterRoleKey
    , RegisterUserKey
    , TestRun
    , TestRunState
    )

data Request k op = Request
    { outputRefId :: RequestRefId
    , owner :: Owner
    , change :: Change k op
    }

deriving instance (Show k, Show (Operation op)) => Show (Request k op)
deriving instance (Eq k, Eq (Operation op)) => Eq (Request k op)

instance
    (Monad m, ToJSON m k, ToJSON m (Operation op))
    => ToJSON m (Request k op)
    where
    toJSON (Request refId owner change) =
        object
            [ "outputRefId" .= refId
            , "owner" .= owner
            , "change" .= change
            ]

instance
    (ReportSchemaErrors m, FromJSON m k, FromJSON m (Operation op))
    => FromJSON m (Request k op)
    where
    fromJSON = withObject "Request" $ \v -> do
        refId <- v .: "outputRefId"
        owner <- v .: "owner"
        change <- v .: "change"
        pure
            $ Request
                { outputRefId = refId
                , owner = owner
                , change = change
                }

data TokenState = TokenState
    { tokenRoot :: Root
    , tokenOwner :: Owner
    }

instance Monad m => ToJSON m TokenState where
    toJSON (TokenState root owner) =
        object
            [ "root" .= root
            , "owner" .= owner
            ]

instance ReportSchemaErrors m => FromJSON m TokenState where
    fromJSON = withObject "TokenState" $ \v -> do
        root <- v .: "root"
        owner <- v .: "owner"
        pure $ TokenState{tokenRoot = root, tokenOwner = owner}

data RequestZoo where
    RegisterUserRequest
        :: Request RegisterUserKey (OpI ()) -> RequestZoo
    UnregisterUserRequest
        :: Request RegisterUserKey (OpD ()) -> RequestZoo
    RegisterRoleRequest
        :: Request RegisterRoleKey (OpI ()) -> RequestZoo
    UnregisterRoleRequest
        :: Request RegisterRoleKey (OpD ()) -> RequestZoo
    CreateTestRequest
        :: Request TestRun (OpI (TestRunState PendingT)) -> RequestZoo
    RejectRequest
        :: Request TestRun (OpU (TestRunState PendingT) (TestRunState DoneT))
        -> RequestZoo
    AcceptRequest
        :: Request TestRun (OpU (TestRunState PendingT) (TestRunState RunningT))
        -> RequestZoo
    FinishedRequest
        :: Request TestRun (OpU (TestRunState RunningT) (TestRunState DoneT))
        -> RequestZoo

requestId :: RequestZoo -> RequestRefId
requestId (RegisterUserRequest req) = outputRefId req
requestId (UnregisterUserRequest req) = outputRefId req
requestId (RegisterRoleRequest req) = outputRefId req
requestId (UnregisterRoleRequest req) = outputRefId req
requestId (CreateTestRequest req) = outputRefId req
requestId (RejectRequest req) = outputRefId req
requestId (AcceptRequest req) = outputRefId req
requestId (FinishedRequest req) = outputRefId req

instance (Alternative m, ReportSchemaErrors m) => FromJSON m RequestZoo where
    fromJSON v = do
        (RegisterUserRequest <$> fromJSON v)
            <|> (UnregisterUserRequest <$> fromJSON v)
            <|> (RegisterRoleRequest <$> fromJSON v)
            <|> (UnregisterRoleRequest <$> fromJSON v)
            <|> (CreateTestRequest <$> fromJSON v)
            <|> (RejectRequest <$> fromJSON v)
            <|> (AcceptRequest <$> fromJSON v)
            <|> (FinishedRequest <$> fromJSON v)

instance Monad m => ToJSON m RequestZoo where
    toJSON (RegisterUserRequest req) = toJSON req
    toJSON (UnregisterUserRequest req) = toJSON req
    toJSON (RegisterRoleRequest req) = toJSON req
    toJSON (UnregisterRoleRequest req) = toJSON req
    toJSON (CreateTestRequest req) = toJSON req
    toJSON (RejectRequest req) = toJSON req
    toJSON (AcceptRequest req) = toJSON req
    toJSON (FinishedRequest req) = toJSON req

data Token = Token
    { tokenRefId :: RequestRefId
    , tokenState :: TokenState
    , tokenRequests :: [RequestZoo]
    }

instance Monad m => ToJSON m Token where
    toJSON (Token refId state requests) =
        object
            [ "outputRefId" .= refId
            , "state" .= state
            , "requests" .= requests
            ]

instance
    (Alternative m, ReportSchemaErrors m)
    => FromJSON m Token
    where
    fromJSON = withObject "Token" $ \v -> do
        refId <- v .: "outputRefId"
        state <- v .: "state"
        requests <- v .: "requests"
        pure
            $ Token
                { tokenRefId = refId
                , tokenState = state
                , tokenRequests = requests
                }

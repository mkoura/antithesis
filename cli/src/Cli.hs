module Cli
    ( cmd
    , Command (..)
    ) where

import Core.Types
    ( RequestRefId
    , TokenId
    , TxHash
    , Wallet
    , WithTxHash (..)
    )
import MPFS.API (getTokenFacts, retractChange)
import Oracle.Cli (OracleCommand (..), oracleCmd)
import Servant.Client (ClientM)
import Submitting (submitting)
import Text.JSON.Canonical (JSValue)
import User.Agent.Cli
    ( AgentCommand (..)
    , IsReady (NotReady)
    , agentCmd
    )
import User.Requester.Cli
    ( RequesterCommand
    , requesterCmd
    )

data Command a where
    RequesterCommand :: RequesterCommand a -> Command a
    OracleCommand :: OracleCommand a -> Command a
    AgentCommand :: AgentCommand NotReady a -> Command (WithTxHash a)
    RetractRequest
        :: { outputReference :: RequestRefId
           }
        -> Command TxHash
    GetFacts :: Command JSValue

deriving instance Show (Command a)
deriving instance Eq (Command a)

cmd :: Wallet -> Maybe TokenId -> Command a -> ClientM a
cmd wallet (Just tokenId) command =
    case command of
        RequesterCommand requesterCommand ->
            requesterCmd wallet tokenId requesterCommand
        OracleCommand oracleCommand -> oracleCmd wallet (Just tokenId) oracleCommand
        AgentCommand agentCommand -> agentCmd wallet tokenId agentCommand
        GetFacts -> getTokenFacts tokenId
        RetractRequest refId -> fmap txHash $ submitting wallet $ \address ->
            retractChange address refId
cmd wallet Nothing command =
    case command of
        RetractRequest refId -> fmap txHash $ submitting wallet $ \address ->
            retractChange address refId
        OracleCommand oracleCommand -> oracleCmd wallet Nothing oracleCommand
        _ -> error "TokenId is required for this command"

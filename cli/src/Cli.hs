module Cli
    ( cmd
    , Command (..)
    ) where

import Control.Monad ((<=<))
import Core.Types
    ( RequestRefId
    , TokenId
    , Wallet
    )
import MPFS.API (getTokenFacts, retractChange)
import Oracle.Cli (OracleCommand (..), oracleCmd)
import Servant.Client (ClientM)
import Submitting (submitting)
import Text.JSON.Canonical (JSValue, ToJSON (..))
import User.Agent.Cli
    ( AgentCommand
    , agentCmd
    )
import User.Requester.Cli
    ( RequesterCommand
    , requesterCmd
    )

data Command
    = RequesterCommand RequesterCommand
    | OracleCommand OracleCommand
    | AgentCommand AgentCommand
    | RetractRequest
        { outputReference :: RequestRefId
        }
    | GetFacts
        {
        }
    deriving (Eq, Show)

cmd :: Wallet -> Maybe TokenId -> Command -> ClientM JSValue
cmd wallet (Just tokenId) command = do
    case command of
        RequesterCommand requesterCommand ->
            requesterCmd wallet tokenId requesterCommand
        OracleCommand oracleCommand -> oracleCmd wallet (Just tokenId) oracleCommand
        AgentCommand agentCommand -> agentCmd wallet tokenId agentCommand
        RetractRequest refId -> toJSON <=< submitting wallet $ \address ->
            retractChange address refId
        GetFacts -> getTokenFacts tokenId
cmd wallet Nothing command = case command of
    OracleCommand oracleCommand -> oracleCmd wallet Nothing oracleCommand
    _ -> error "TokenId is required for this command"

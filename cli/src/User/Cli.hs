module User.Cli
    ( userCmd
    , UserCommand (..)
    ) where

import Control.Monad ((<=<))
import Core.Types
    ( RequestRefId
    , TokenId
    , Wallet (..)
    )
import MPFS.API
    ( getTokenFacts
    , retractChange
    )
import Servant.Client (ClientM)
import Submitting (submittingFake)
import Text.JSON.Canonical (JSValue, ToJSON (..))
import User.Requester.Cli (RequesterCommand, requesterCmd)

data Operation = Insert | Delete
    deriving (Eq, Show)

data UserCommand
    = UserRequesterCommand RequesterCommand
    | RetractRequest
        { outputReference :: RequestRefId
        }
    | GetFacts
        {
        }
    deriving (Eq, Show)

userCmd :: Wallet -> TokenId -> UserCommand -> ClientM JSValue
userCmd wallet tokenId command = do
    case command of
        UserRequesterCommand requesterCommand -> requesterCmd wallet tokenId requesterCommand
        RetractRequest refId -> toJSON <=< submittingFake wallet $ \address ->
            retractChange address refId
        GetFacts -> getTokenFacts tokenId

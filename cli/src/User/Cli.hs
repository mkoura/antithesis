module User.Cli
    ( userCmd
    , UserCommand (..)
    ) where

import Core.Types
    ( RequestRefId
    , TokenId
    , Wallet (..)
    )
import Data.Aeson (ToJSON (..), Value (..))
import MPFS.API
    ( getTokenFacts
    , retractChange
    )
import Servant.Client (ClientM)
import Submitting (submittingFake)
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

userCmd :: Wallet -> TokenId -> UserCommand -> ClientM Value
userCmd wallet tokenId command = do
    case command of
        UserRequesterCommand requesterCommand -> requesterCmd wallet tokenId requesterCommand
        RetractRequest refId -> fmap toJSON $ submittingFake wallet $ \address ->
            retractChange address refId
        GetFacts -> getTokenFacts tokenId

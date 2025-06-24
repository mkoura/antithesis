module User.Cli
    ( userCmd
    ) where

import Data.Aeson (ToJSON (..), Value (..))
import MPFS.API
    ( getTokenFacts
    , retractChange
    )
import Servant.Client (ClientM)
import Submitting (submittingFake)
import Types
    ( TokenId
    , UserCommand (..)
    , Wallet (..)
    )
import User.Requester.Cli (requesterCmd)

data Operation = Insert | Delete
    deriving (Eq, Show)

userCmd :: Wallet -> TokenId -> UserCommand -> ClientM Value
userCmd wallet tokenId command = do
    case command of
        UserRequesterCommand requesterCommand -> requesterCmd wallet tokenId requesterCommand
        RetractRequest refId -> fmap toJSON $ submittingFake wallet $ \address ->
            retractChange address refId
        GetFacts -> getTokenFacts tokenId

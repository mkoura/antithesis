{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Oracle.Github.GetRepoRoleIO
    ( ResponseCodeownersFile (..)
    , downloadCodeownersFile
    ) where

import Control.Lens ((&), (.~), (^.))
import Core.Types (Repository (..))
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import Network.Wreq qualified as Wreq

-- In order to verify the role of the userX CODEOWNERS file is downloaded with
-- the expectation there a line:
-- role: user1 user2 .. userX .. userN
newtype ResponseCodeownersFile = ResponseCodeownersFile
    { file :: ByteString
    }
    deriving (Eq, Generic, Show)

downloadCodeownersFile
    :: Repository
    -> IO ResponseCodeownersFile
downloadCodeownersFile repo = do
    response <- Wreq.getWith headers endpoint
    case response ^. Wreq.responseStatus . Wreq.statusCode of
        200 ->
            pure
                $ ResponseCodeownersFile (response ^. Wreq.responseBody)
        _ ->
            error
                $ "There is no CODEOWNERS file in "
                    <> show (organization repo)
                    <> "/"
                    <> show (project repo)
                    <> " github repository, which is required to "
                    <> "verify the role of user in the repository."
  where
    headers =
        Wreq.defaults
            & Wreq.header "Accept" .~ ["application/vnd.github.raw+json"]
            & Wreq.header "X-GitHub-Api-Version" .~ ["2022-11-28"]
    endpoint =
        "https://api.github.com/repos/"
            <> organization repo
            <> "/"
            <> project repo
            <> "/contents/CODEOWNERS"

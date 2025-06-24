{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Oracle.Github.GetRepoRoleIO
    ( ResponseCodeownersFile (..)
    , downloadCodeownersFile
    ) where

import Control.Lens ((^.))
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import Types (Repository (..), Username (..))

import qualified Network.Wreq as Wreq

-- In order to verify the role of the userX CODEOWNERS file is downloaded with
-- the expectation there a line:
-- role: user1 user2 .. userX .. userN
newtype ResponseCodeownersFile = ResponseCodeownersFile
    { file :: ByteString
    }
    deriving (Eq, Generic, Show)

downloadCodeownersFile
    :: Username
    -> Repository
    -> IO ResponseCodeownersFile
downloadCodeownersFile (Username user) repo = do
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
                    <> "verify the role of "
                    <> user
                    <> "in the repository."
  where
    headers = Wreq.defaults
    endpoint =
        "https://raw.githubusercontent.com/"
            <> organization repo
            <> "/"
            <> project repo
            <> "/master/CODEOWNERS"

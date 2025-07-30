import E2ESpec (e2eSpec)
import MPFS.APISpec
import Lib.Github.OracleValidationSpec
import Test.Hspec
import GitHub (Auth (..))
import qualified Data.ByteString.Char8 as BC
import System.Environment (lookupEnv)

main :: IO ()
main = hspec $ do
    beforeAll getPAT $ do
        e2eSpec
        mpfsAPISpec
        roleSpecs
    userSpec


tryGetPAT :: IO (Maybe Auth)
tryGetPAT = fmap (OAuth . BC.pack) <$> lookupEnv "GITHUB_PERSONAL_ACCESS_TOKEN"

getPAT :: IO Auth
getPAT = do
    mpat <- tryGetPAT
    case mpat of
        Just pat -> return pat
        Nothing -> error "Environment variable ANTI_GITHUB_PAT is not set. \
            \ Please set it to some valid GitHub Personal Access Token with \
            \ read access to public repositories."
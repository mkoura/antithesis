{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}
module Facts
    ( FactsSelection (..)
    , TestRunSelection (..)
    , factsCmd
    , All (..)
    , tryDecryption
    , URLDecryptionIssue (..)
    )
where

import Control.Arrow (left)
import Control.Monad (filterM, when)
import Core.Types.Basic (TokenId, Username)
import Core.Types.Fact (Fact (..), keyHash, parseFacts)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as B8
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Lib.CryptoBox (decryptOnly)
import Lib.CryptoBox qualified as CB
import Lib.JSON.Canonical.Extra (blakeHashOfJSON)
import Lib.SSH.Private (KeyAPI (..), SSHClient)
import Lib.SSH.Public (decodePublicKey)
import MPFS.API (MPFS, mpfsGetTokenFacts)
import Oracle.Config.Types (Config, ConfigKey)
import Text.JSON.Canonical (FromJSON, JSValue, ToJSON)
import User.Agent.Types (TestRunId (..), WhiteListKey)
import User.Types
    ( Phase (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunState (..)
    , URL (..)
    )
import Validation (Validation)

data All = All | Requester Username
    deriving (Eq, Show)
data TestRunSelection a where
    TestRunPending
        :: [TestRunId]
        -> All
        -> TestRunSelection [Fact TestRun (TestRunState 'PendingT)]
    TestRunRunning
        :: [TestRunId]
        -> All
        -> TestRunSelection [Fact TestRun (TestRunState 'RunningT)]
    TestRunDone
        :: [TestRunId]
        -> All
        -> TestRunSelection [Fact TestRun (TestRunState 'DoneT)]
    TestRunRejected
        :: [TestRunId]
        -> All
        -> TestRunSelection [Fact TestRun (TestRunState 'DoneT)]
    AnyTestRuns
        :: [TestRunId]
        -> All
        -> TestRunSelection [Fact TestRun JSValue]
data FactsSelection a where
    UserFacts :: FactsSelection [Fact RegisterUserKey ()]
    RoleFacts :: FactsSelection [Fact RegisterUserKey ()]
    TestRunFacts :: TestRunSelection a -> FactsSelection a
    ConfigFact :: FactsSelection [Fact ConfigKey Config]
    WhiteListedFacts :: FactsSelection [Fact WhiteListKey ()]
    AllFacts :: FactsSelection [Fact JSValue JSValue]

retrieveAnyFacts
    :: (FromJSON Maybe k, FromJSON Maybe v, Functor m)
    => MPFS m
    -> TokenId
    -> m [Fact k v]
retrieveAnyFacts mpfs tokenId = parseFacts <$> mpfsGetTokenFacts mpfs tokenId

filterFacts
    :: (Foldable t, ToJSON Identity k)
    => t TestRunId
    -> [Fact k v]
    -> [Fact k v]
filterFacts ids
    | null ids = id
    | otherwise =
        runIdentity
            . filterM (\v -> (`elem` ids) . TestRunId <$> keyHash (factKey v))

whoseFilter :: All -> [Fact TestRun v] -> [Fact TestRun v]
whoseFilter whose facts = filterOn facts factKey
    $ \case
        TestRun{requester} -> case whose of
            All -> True
            Requester u -> requester == u

factsCmd
    :: forall m a
     . Monad m
    => Maybe (SSHClient, Validation m)
    -> MPFS m
    -> TokenId
    -> FactsSelection a
    -> m a
factsCmd _ mpfs tokenId UserFacts = retrieveAnyFacts mpfs tokenId
factsCmd _ mpfs tokenId RoleFacts = retrieveAnyFacts mpfs tokenId
factsCmd _ mpfs tokenId (TestRunFacts (TestRunPending ids whose)) = do
    retrieveAnyFacts mpfs tokenId <&> filterFacts ids . whoseFilter whose
factsCmd _ mpfs tokenId (TestRunFacts (TestRunRunning ids whose)) = do
    retrieveAnyFacts mpfs tokenId <&> filterFacts ids . whoseFilter whose
factsCmd _ mpfs tokenId (TestRunFacts (TestRunDone ids whose)) = do
    facts <-
        retrieveAnyFacts mpfs tokenId <&> filterFacts ids . whoseFilter whose
    pure
        $ filter
            ( \v -> case factValue v of
                Finished{} -> True
                _ -> False
            )
            facts
factsCmd _ mpfs tokenId (TestRunFacts (TestRunRejected ids whose)) = do
    facts <-
        retrieveAnyFacts mpfs tokenId <&> filterFacts ids . whoseFilter whose
    pure
        $ filter
            ( \v -> case factValue v of
                Rejected{} -> True
                _ -> False
            )
            facts
factsCmd _ mpfs tokenId (TestRunFacts (AnyTestRuns ids whose)) =
    retrieveAnyFacts mpfs tokenId <&> filterFacts ids . whoseFilter whose
factsCmd _ mpfs tokenId ConfigFact = retrieveAnyFacts mpfs tokenId
factsCmd _ mpfs tokenId WhiteListedFacts = retrieveAnyFacts mpfs tokenId
factsCmd _ mpfs tokenId AllFacts = retrieveAnyFacts mpfs tokenId

filterOn :: [a] -> (a -> b) -> (b -> Bool) -> [a]
filterOn xs f p = filter (p . f) xs

data URLDecryptionIssue
    = StateIsNotFinished
    | SSHKeyDoesNotApply
    | PublicKeyNotDecodable
    | URLNotBase64 String
    | NonceNotCreatable
    | KeyConversionsFailed String
    | URLDecryptionFailed
    | UsersNotRegistered Username

nothingLeft :: e -> Maybe a -> Either e a
nothingLeft e = maybe (Left e) Right

tryDecryption
    :: [RegisterUserKey]
    -> Maybe KeyAPI
    -> Fact TestRun (TestRunState 'DoneT)
    -> Fact TestRun (TestRunState 'DoneT)
tryDecryption _ Nothing f = f
tryDecryption registeredUsers (Just kapi) f@(Fact tr ts) =
    case decryptURL registeredUsers tr ts kapi of
        Left _ -> f
        Right ts' -> Fact{factKey = tr, factValue = ts'}

decryptURL
    :: [RegisterUserKey]
    -> TestRun
    -> TestRunState DoneT
    -> KeyAPI
    -> Either URLDecryptionIssue (TestRunState DoneT)
decryptURL _ _ Rejected{} _ = Left StateIsNotFinished
decryptURL
    users
    testRun@TestRun{requester}
    (Finished old dur (URL enc))
    KeyAPI{privateKey, publicKey = sshPublicKey} = do
        RegisterUserKey{pubkeyhash} <-
            nothingLeft (UsersNotRegistered requester)
                $ find ((== requester) . username) users
        (_, publicKey) <-
            nothingLeft PublicKeyNotDecodable $ decodePublicKey pubkeyhash
        when (publicKey /= sshPublicKey) $ Left SSHKeyDoesNotApply
        decodedURL <- left URLNotBase64 $ Base64.decode $ B8.pack enc
        nonce <-
            nothingLeft NonceNotCreatable
                $ CB.mkNonce
                $ runIdentity
                $ blakeHashOfJSON testRun
        murl <-
            left KeyConversionsFailed $ decryptOnly privateKey decodedURL nonce
        url <- nothingLeft URLDecryptionFailed murl
        pure $ Finished old dur $ URL $ B8.unpack url

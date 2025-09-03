module Adversary (adversary, Message (..), toString, readChainPoint, originPoint) where

import Adversary.ChainSync (clientChainSync, Point, HeaderHash)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Encoding qualified as T
import Data.Text qualified as T
import GHC.Generics (Generic)
import Ouroboros.Network.Magic (NetworkMagic(..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (WithOrigin(..))
import Ouroboros.Network.Block (SlotNo(..) )
import qualified Ouroboros.Network.Point as Point
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import qualified Data.ByteString.Short as SBS

originPoint :: Point
originPoint = Network.Point Origin

newtype Message = Startup {arguments :: [String]}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

adversary :: [String] -> IO Message
adversary args@(magicArg : host : port : limitArg : startPointArg : _) = do
  let magic = NetworkMagic { unNetworkMagic = read magicArg }
  let (_startPoint :: Point) = fromMaybe (error "invalid chain point") $ readChainPoint startPointArg
  clientChainSync magic host (read port) (read limitArg)
  pure Startup {arguments = args}
adversary _ = error "Expected network-magic, host, port, sync-length and startPoint arguments"

toString :: Message -> String
toString = TL.unpack . TL.decodeUtf8 . Aeson.encode

readChainPoint :: String -> Maybe Point
readChainPoint "origin" = Just originPoint
readChainPoint str = case split (== '@') str of
    [blockHashStr, slotNoStr] -> do
        let (hash :: HeaderHash) = Consensus.OneEraHash $ SBS.toShort $ T.encodeUtf8 $ T.pack blockHashStr
        slot <- SlotNo <$> readMaybe slotNoStr
        return $ Network.Point $ At $ Point.Block slot hash
    _ -> Nothing
  where
    split f = map T.unpack . T.split f . T.pack

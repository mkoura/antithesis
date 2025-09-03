{-# OPTIONS_GHC -Wno-orphans #-}

module Adversary (adversary, Message (..), toString, readChainPoint, originPoint) where

import Adversary.ChainSync (clientChainSync, Point, HeaderHash)
import Data.Aeson (FromJSON, ToJSON, withText)
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
import Data.ByteString.Base16 qualified as B16

originPoint :: Point
originPoint = Network.Point Origin

data Message
    = Startup {arguments :: [String]}
    | Completed {startPoint :: Point, endPoint :: Point}
    | Failed {reason :: String}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToJSON Point where
    toJSON = Aeson.toJSON . showChainPoint

instance FromJSON Point where
    parseJSON = withText "point" $ \t ->
        maybe
            (fail $ "not a point: " <> T.unpack t)
            pure
            (readChainPoint $ T.unpack t)

adversary :: [String] -> IO Message
adversary args@(magicArg : host : port : limitArg : startPointArg : _) = do
  putStrLn $ toString $ Startup args
  let magic = NetworkMagic { unNetworkMagic = read magicArg }
  let (startPoint :: Point) = fromMaybe (error "invalid chain point") $ readChainPoint startPointArg
  res <- clientChainSync magic host (read port) startPoint (read limitArg)
  case res of
    Right endPoint -> pure $ Completed{startPoint, endPoint}
    Left err -> pure $ Failed $ show err
adversary _ = error "Expected network-magic, host, port, sync-length and startPoint arguments"

toString :: Message -> String
toString = TL.unpack . TL.decodeUtf8 . Aeson.encode

readChainPoint :: String -> Maybe Point
readChainPoint "origin" = Just originPoint
readChainPoint str = case split (== '@') str of
    [blockHashStr, slotNoStr] -> do
        (hash :: HeaderHash) <- Consensus.OneEraHash . SBS.toShort <$> (either (const Nothing) Just $ B16.decode $ T.encodeUtf8 $ T.pack blockHashStr)
        slot <- SlotNo <$> readMaybe slotNoStr
        return $ Network.Point $ At $ Point.Block slot hash
    _ -> Nothing
  where
    split f = map T.unpack . T.split f . T.pack

showChainPoint :: Point -> String
showChainPoint (Network.Point Origin) = "origin"
showChainPoint (Network.Point (At (Point.Block (SlotNo slot) hash)))
    = show hash <> "@" <> show slot

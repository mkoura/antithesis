{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Adversary.ChainSync (clientChainSync, Limit (..)) where

import Cardano.Chain.Slotting (EpochSlots (EpochSlots))
import Codec.Serialise qualified as CBOR
import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM (atomically, STM),
    StrictTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )
import Control.Tracer (Contravariant (contramap), stdoutTracer)
import Data.ByteString.Lazy qualified as LBS
import Data.Data (Proxy (Proxy))
import Data.Functor (void)
import Data.List.NonEmpty qualified as NE
import Data.Void (Void)
import Network.Mux qualified as Mx
import Network.Socket
  ( AddrInfo (..),
    AddrInfoFlag (AI_PASSIVE),
    PortNumber,
    SocketType (Stream),
    defaultHints,
    getAddrInfo,
  )
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.Block.Abstract (decodeRawHash, encodeRawHash)
import Ouroboros.Consensus.Byron.Ledger (CodecConfig (..))
import Ouroboros.Consensus.Cardano.Block (CodecConfig (CardanoCodecConfig))
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Cardano.Node (pattern CardanoNodeToNodeVersion2)
import Ouroboros.Consensus.Node.Serialisation (decodeNodeToNode, encodeNodeToNode)
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger (CodecConfig (ShelleyCodecConfig))
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block
  ( castPoint,
    decodeTip,
    encodeTip,
  )
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Diffusion.Configuration
  ( PeerSharing (PeerSharingDisabled),
  )
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mock.Chain (Chain)
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.Mux
  ( MiniProtocol (..),
    MiniProtocolLimits (..),
    MiniProtocolNum (MiniProtocolNum),
    OuroborosApplication (OuroborosApplication),
    OuroborosApplicationWithMinimalCtx,
    RunMiniProtocol (InitiatorProtocolOnly),
    RunMiniProtocolWithMinimalCtx,
    StartOnDemandOrEagerly (StartOnDemand),
    mkMiniProtocolCbFromPeer,
  )
import Ouroboros.Network.NodeToNode
  ( ControlMessage (..),
    ControlMessageSTM,
    DiffusionMode (InitiatorOnlyDiffusionMode),
    NodeToNodeVersion (NodeToNodeV_14),
    NodeToNodeVersionData (..),
    nodeToNodeCodecCBORTerm,
  )
import Ouroboros.Network.Protocol.ChainSync.Client
  ( ChainSyncClient (..),
    ClientStIdle (..),
    ClientStIntersect (..),
    ClientStNext (..),
  )
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Codec qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Protocol.Handshake.Codec
  ( cborTermVersionDataCodec,
    noTimeLimitsHandshake,
    nodeToNodeHandshakeCodec,
  )
import Ouroboros.Network.Protocol.Handshake.Version
  ( Acceptable (acceptableVersion),
    Queryable (queryVersion),
    simpleSingletonVersions,
  )
import Ouroboros.Network.Snocket
  ( makeSocketBearer,
    socketSnocket,
  )
import Ouroboros.Network.Socket
  ( ConnectToArgs (..),
    HandshakeCallbacks (HandshakeCallbacks),
    connectToNode,
    nullNetworkConnectTracers,
  )
import Data.Word (Word32)

type Block = Consensus.CardanoBlock Consensus.StandardCrypto

type Header = Consensus.Header Block

type Tip = Network.Tip Block

type Point = Network.Point Header

newtype Limit = Limit { limit :: Word32 }
  deriving newtype (Show, Read, Eq, Ord)

clientChainSync ::
  NetworkMagic ->
  String ->
  PortNumber ->
  Limit ->
  IO ()
clientChainSync magic peerName peerPort limit = withIOManager $ \iocp -> do
  AddrInfo {addrAddress} <- resolve
  var <- newTVarIO (Chain.Genesis :: Chain Header)
  void $
    connectToNode
      (socketSnocket iocp)
      makeSocketBearer
      ConnectToArgs
        { ctaHandshakeCodec = nodeToNodeHandshakeCodec,
          ctaHandshakeTimeLimits = noTimeLimitsHandshake,
          ctaVersionDataCodec = cborTermVersionDataCodec nodeToNodeCodecCBORTerm,
          ctaConnectTracers = nullNetworkConnectTracers,
          ctaHandshakeCallbacks =
            HandshakeCallbacks acceptableVersion queryVersion
        }
      mempty
      ( simpleSingletonVersions
          NodeToNodeV_14
          ( NodeToNodeVersionData
              { networkMagic = magic,
                diffusionMode = InitiatorOnlyDiffusionMode,
                peerSharing = PeerSharingDisabled,
                query = False
              }
          )
          (\_ -> app var)
      )
      Nothing
      addrAddress
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      NE.head
        <$> getAddrInfo (Just hints) (Just peerName) (Just $ show peerPort)

    app ::
      StrictTVar IO (Chain Header) ->
      OuroborosApplicationWithMinimalCtx Mx.InitiatorMode addr LBS.ByteString IO () Void
    app var = demoProtocol2 $
      InitiatorProtocolOnly $
        mkMiniProtocolCbFromPeer $
          \_ctx ->
            ( contramap show stdoutTracer,
              codecChainSync,
              ChainSync.chainSyncClientPeer (client var)
            )

    client :: StrictTVar IO (Chain Header) -> ChainSyncClient Header Point Tip IO ()
    client var = chainSyncClient var (controlledClient $ terminateAfterCount var)

    terminateAfterCount :: StrictTVar IO (Chain Header) -> STM IO ControlMessage
    terminateAfterCount var = do
      chainLength <- getChainLength var
      pure $ if chainLength < limit
                then Continue
                else Terminate

    getChainLength :: StrictTVar IO (Chain Header) -> STM IO Limit
    getChainLength var = Limit . fromIntegral . Chain.length <$> readTVar var

-- TODO: provide sensible limits
-- https://github.com/intersectmbo/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
  MiniProtocolLimits
    { maximumIngressQueue = maxBound
    }

--
-- Chain sync demo
--

demoProtocol2 ::
  -- | chainSync
  RunMiniProtocolWithMinimalCtx appType addr bytes m a b ->
  OuroborosApplicationWithMinimalCtx appType addr bytes m a b
demoProtocol2 chainSync =
  OuroborosApplication
    [ MiniProtocol
        { miniProtocolNum = MiniProtocolNum 2,
          miniProtocolStart = StartOnDemand,
          miniProtocolLimits = maximumMiniProtocolLimits,
          miniProtocolRun = chainSync
        }
    ]

data Client header point tip m t = Client
  { rollbackward ::
      point ->
      tip ->
      m (Either t (Client header point tip m t)),
    rollforward :: header -> m (Either t (Client header point tip m t)),
    points :: [point] -> m (Either t (Client header point tip m t))
  }

controlledClient ::
  (MonadSTM m) =>
  ControlMessageSTM m ->
  Client header point tip m ()
controlledClient controlMessageSTM = client
  where
    client =
      Client
        { rollbackward = \_ _ -> do
            ctrl <- atomically controlMessageSTM
            case ctrl of
              Continue -> pure (Right client)
              Quiesce ->
                error
                  "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
              Terminate -> pure (Left ()),
          rollforward = \_ -> do
            ctrl <- atomically controlMessageSTM
            case ctrl of
              Continue -> pure (Right client)
              Quiesce ->
                error
                  "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
              Terminate -> pure (Left ()),
          points = \_ -> pure (Right client)
        }

chainSyncClient ::
  StrictTVar IO (Chain Header) ->
  Client Header Point Tip IO a ->
  ChainSyncClient Header Point Tip IO a
chainSyncClient chainvar client =
  ChainSyncClient $
    either SendMsgDone initialise <$> getChainPoints
  where
    initialise (points, client') =
      SendMsgFindIntersect points $
        -- In this consumer example, we do not care about whether the server
        -- found an intersection or not. If not, we'll just sync from genesis.
        --
        -- Alternative policies here include:
        --  iteratively finding the best intersection
        --  rejecting the server if there is no intersection in the last K blocks
        --
        ClientStIntersect
          { recvMsgIntersectFound = \_ _ -> ChainSyncClient (return (requestNext client')),
            recvMsgIntersectNotFound = \_ -> ChainSyncClient (return (requestNext client'))
          }

    requestNext client' =
      SendMsgRequestNext
        -- We have the opportunity to do something when receiving
        -- MsgAwaitReply. In this example we don't take up that opportunity.
        (pure ())
        (handleNext client')

    handleNext client' =
      ClientStNext
        { recvMsgRollForward = \header _tip -> ChainSyncClient $ do
            addBlock header
            choice <- rollforward client' header
            pure $ case choice of
              Left a -> SendMsgDone a
              Right client'' -> requestNext client'',
          recvMsgRollBackward = \pIntersect tip -> ChainSyncClient $ do
            rollback pIntersect
            choice <- rollbackward client' pIntersect tip
            pure $ case choice of
              Left a -> SendMsgDone a
              Right client'' -> requestNext client''
        }

    getChainPoints = do
      pts <-
        Chain.selectPoints [] <$> readTVarIO chainvar
      choice <- points client (fmap castPoint pts)
      pure $ case choice of
        Left a -> Left a
        Right client' -> Right (fmap castPoint pts, client')

    addBlock b = atomically $ do
      chain <- readTVar chainvar
      let !chain' = Chain.addBlock b chain
      writeTVar chainvar chain'

    rollback p = atomically $ do
      chain <- readTVar chainvar
      -- TODO: handle rollback failure
      let !chain' = case Chain.rollback (castPoint p) chain of
            Just a -> a
            Nothing -> error "out of scope rollback"
      writeTVar chainvar chain'

codecChainSync ::
  Codec
    (ChainSync.ChainSync Header Point Tip)
    CBOR.DeserialiseFailure
    IO
    LBS.ByteString
codecChainSync =
  ChainSync.codecChainSync enc dec CBOR.encode CBOR.decode encTip decTip
  where
    --    enc :: SerialiseNodeToNode blk a => a -> Encoding
    enc = encodeNodeToNode @Block ccfg version
    dec = decodeNodeToNode @Block ccfg version

    encTip = encodeTip (encodeRawHash (Proxy @Block))
    decTip = decodeTip (decodeRawHash (Proxy @Block))
    ccfg =
      CardanoCodecConfig
        (ByronCodecConfig $ EpochSlots 42)
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig

    version = CardanoNodeToNodeVersion2 -- @Block

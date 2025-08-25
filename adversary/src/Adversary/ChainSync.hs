{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Adversary.ChainSync where

import Cardano.Chain.Slotting
import Adversary.ChainSyncOld (recentOffsets)
import Codec.Serialise qualified as CBOR
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (atomically)
    , StrictTVar
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTVar
    )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Data.Void (Void)
import Ouroboros.Consensus.Cardano
    ( CardanoBlock, ProtVer (..), ShelleyGenesis
    )
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Control.Tracer (Contravariant (contramap), stdoutTracer, nullTracer)
import Data.ByteString.Lazy qualified as LBS
import Data.Functor (void)
import Data.List.NonEmpty qualified as NE
import Network.Socket
    ( AddrInfo (AddrInfo, addrAddress, addrFlags, addrSocketType)
    , AddrInfoFlag (AI_PASSIVE)
    , PortNumber
    , SocketType (Stream)
    , defaultHints
    , getAddrInfo
    )
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Network.Block
    ( HasHeader
    , HeaderHash
    , castPoint, encodeTip, decodeTip
    )
import Ouroboros.Network.Diffusion.Configuration
    ( PeerSharing (PeerSharingDisabled)
    )
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mock.Chain (Chain)
import Ouroboros.Consensus.Node.Serialisation (encodeNodeToNode, decodeNodeToNode)
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.Mux
    ( MiniProtocol
        ( MiniProtocol
        , miniProtocolLimits
        , miniProtocolNum
        , miniProtocolRun
        , miniProtocolStart
        )
    , MiniProtocolLimits (..)
    , MiniProtocolNum (MiniProtocolNum)
    , OuroborosApplication (OuroborosApplication)
    , OuroborosApplicationWithMinimalCtx
    , RunMiniProtocol (InitiatorProtocolOnly)
    , RunMiniProtocolWithMinimalCtx
    , StartOnDemandOrEagerly (StartOnDemand)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.NodeToNode
    ( ControlMessage (..)
    , ControlMessageSTM
    , DiffusionMode (InitiatorOnlyDiffusionMode)
    , NodeToNodeVersion (NodeToNodeV_14)
    , NodeToNodeVersionData (..)
    , nodeToNodeCodecCBORTerm
    )
import Ouroboros.Network.Protocol.ChainSync.Client
    ( ChainSyncClient (..)
    , ClientStIdle (..)
    , ClientStIntersect (..)
    , ClientStNext (..)
    )
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Codec qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Protocol.Handshake.Version
    ( Acceptable (acceptableVersion)
    , Queryable (queryVersion)
    , simpleSingletonVersions
    )
import Ouroboros.Network.Protocol.Handshake.Codec
    ( cborTermVersionDataCodec
    , noTimeLimitsHandshake
    , nodeToNodeHandshakeCodec
    )
import Ouroboros.Network.Snocket
    ( makeSocketBearer
    , socketSnocket
    )
import Ouroboros.Network.Socket
    ( ConnectToArgs
        ( ConnectToArgs
        , ctaConnectTracers
        , ctaHandshakeCallbacks
        , ctaHandshakeCodec
        , ctaHandshakeTimeLimits
        , ctaVersionDataCodec
        )
    , HandshakeCallbacks (HandshakeCallbacks)
    , connectToNode
    , nullNetworkConnectTracers
    )
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import Network.Mux qualified as Mx
import Ouroboros.Network.ControlMessage (continueForever)
import Data.Data (Proxy(Proxy))
import qualified Ouroboros.Network.Block as Network
import Ouroboros.Consensus.Shelley.Ledger (mkShelleyBlockConfig, ShelleyNodeToNodeVersion (ShelleyNodeToNodeVersion1), CodecConfig (ShelleyCodecConfig))
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Cardano.Block (ConwayEra, StandardCrypto, CodecConfig (CardanoCodecConfig))
import Ouroboros.Consensus.Cardano.Node (pattern CardanoNodeToNodeVersion2)
import Ouroboros.Consensus.Block.Abstract (encodeRawHash, decodeRawHash)
import Ouroboros.Consensus.Byron.Ledger (mkByronCodecConfig, CodecConfig (..))

type Block = Consensus.CardanoBlock Consensus.StandardCrypto
type Header = Consensus.Header Block
type Tip = Network.Tip Block
type Point = Network.Point Header

clientChainSync
    :: String
    -> PortNumber
    -> ProtVer
    -> ShelleyGenesis
    -> IO ()
clientChainSync peerName peerPort protVer shelleyGenesis = withIOManager $ \iocp -> do
    AddrInfo{addrAddress} <- resolve
    var <- newTVarIO (Chain.Genesis :: Chain Header)
    void
        $ connectToNode
            (socketSnocket iocp)
            makeSocketBearer
            ConnectToArgs
                { ctaHandshakeCodec = nodeToNodeHandshakeCodec
                , ctaHandshakeTimeLimits = noTimeLimitsHandshake
                , ctaVersionDataCodec = cborTermVersionDataCodec nodeToNodeCodecCBORTerm
                , ctaConnectTracers = nullNetworkConnectTracers
                , ctaHandshakeCallbacks =
                    HandshakeCallbacks acceptableVersion queryVersion
                }
            mempty
            ( simpleSingletonVersions
                NodeToNodeV_14
                ( NodeToNodeVersionData
                    { networkMagic = NetworkMagic 1
                    , diffusionMode = InitiatorOnlyDiffusionMode
                    , peerSharing = PeerSharingDisabled
                    , query = False
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
                    { addrFlags = [AI_PASSIVE]
                    , addrSocketType = Stream
                    }
        NE.head
            <$> getAddrInfo (Just hints) (Just peerName) (Just $ show peerPort)

    app
        :: StrictTVar IO (Chain Header)
        -> OuroborosApplicationWithMinimalCtx Mx.InitiatorMode addr LBS.ByteString IO () Void
    app var = demoProtocol2
        $ InitiatorProtocolOnly
        $ mkMiniProtocolCbFromPeer
        $ \_ctx ->
            ( nullTracer --contramap show stdoutTracer
            , codecChainSync protVer shelleyGenesis
            , ChainSync.chainSyncClientPeer (client var)
            )

    client :: StrictTVar IO (Chain Header) -> ChainSyncClient Header Point Tip IO ()
    client var = chainSyncClient var (controlledClient $ continueForever (Proxy @IO))


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

demoProtocol2
    :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b
    -- ^ chainSync
    -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
demoProtocol2 chainSync =
    OuroborosApplication
        [ MiniProtocol
            { miniProtocolNum = MiniProtocolNum 2
            , miniProtocolStart = StartOnDemand
            , miniProtocolLimits = maximumMiniProtocolLimits
            , miniProtocolRun = chainSync
            }
        ]

data Client header point tip m t = Client
    { rollbackward
        :: point
        -> tip
        -> m (Either t (Client header point tip m t))
    , rollforward :: header -> m (Either t (Client header point tip m t))
    , points :: [point] -> m (Either t (Client header point tip m t))
    }

controlledClient
    :: (MonadSTM m)
    => ControlMessageSTM m
    -> Client header point tip m ()
controlledClient controlMessageSTM = go
  where
    go =
        Client
            { rollbackward = \_ _ -> do
                ctrl <- atomically controlMessageSTM
                case ctrl of
                    Continue -> pure (Right go)
                    Quiesce ->
                        error
                            "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
                    Terminate -> pure (Left ())
            , rollforward = \_ -> do
                ctrl <- atomically controlMessageSTM
                case ctrl of
                    Continue -> pure (Right go)
                    Quiesce ->
                        error
                            "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
                    Terminate -> pure (Left ())
            , points = \_ -> pure (Right go)
            }

chainSyncClient
    :: StrictTVar IO (Chain Header)
    -> Client Header Point Tip IO a
    -> ChainSyncClient Header Point Tip IO a
chainSyncClient chainvar client =
    ChainSyncClient
        $ either SendMsgDone initialise <$> getChainPoints
  where
    initialise (points, client') =
        SendMsgFindIntersect points
            $
            -- In this consumer example, we do not care about whether the server
            -- found an intersection or not. If not, we'll just sync from genesis.
            --
            -- Alternative policies here include:
            --  iteratively finding the best intersection
            --  rejecting the server if there is no intersection in the last K blocks
            --
            ClientStIntersect
                { recvMsgIntersectFound = \_ _ -> ChainSyncClient (return (requestNext client'))
                , recvMsgIntersectNotFound = \_ -> ChainSyncClient (return (requestNext client'))
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
                    Right client'' -> requestNext client''
            , recvMsgRollBackward = \pIntersect tip -> ChainSyncClient $ do
                rollback pIntersect
                choice <- rollbackward client' pIntersect tip
                pure $ case choice of
                    Left a -> SendMsgDone a
                    Right client'' -> requestNext client''
            }

    getChainPoints = do
        pts <-
            Chain.selectPoints recentOffsets <$> readTVarIO chainvar
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

-- chainSyncClient ::
--   ControlMessageSTM IO ->
--   Maybe SlotNo ->
--   ChainSync.ChainSyncClient
--     BlockHeader
--     (Point BlockHeader)
--     (Point BlockHeader)
--     IO
--     ()
-- chainSyncClient controlMessageSTM maxSlotNo =
--   ChainSync.ChainSyncClient $ do
--     curvar <- newTVarIO genesisAnchoredFragment
--     chainvar <- newTVarIO genesisAnchoredFragment
--     case chainSyncClient' controlMessageSTM maxSlotNo nullTracer curvar chainvar of
--       ChainSync.ChainSyncClient k -> k

-- chainSyncClient' ::
--   ControlMessageSTM IO ->
--   Maybe SlotNo ->
--   Tracer IO (Point BlockHeader, Point BlockHeader) ->
--   StrictTVar IO (AF.AnchoredFragment BlockHeader) ->
--   StrictTVar IO (AF.AnchoredFragment BlockHeader) ->
--   ChainSync.ChainSyncClient
--     BlockHeader
--     (Point BlockHeader)
--     (Point BlockHeader)
--     IO
--     ()
-- chainSyncClient' controlMessageSTM _maxSlotNo syncTracer _currentChainVar candidateChainVar =
--   ChainSync.ChainSyncClient (return requestNext)
--   where
--     requestNext ::
--       ChainSync.ClientStIdle
--         BlockHeader
--         (Point BlockHeader)
--         (Point BlockHeader)
--         IO
--         ()
--     requestNext =
--       ChainSync.SendMsgRequestNext
--         (pure ()) -- on MsgAwaitReply; could trace
--         handleNext

--     terminate ::
--       ChainSync.ClientStIdle
--         BlockHeader
--         (Point BlockHeader)
--         (Point BlockHeader)
--         IO
--         ()
--     terminate = ChainSync.SendMsgDone ()

--     handleNext ::
--       ChainSync.ClientStNext
--         BlockHeader
--         (Point BlockHeader)
--         (Point BlockHeader)
--         IO
--         ()
--     handleNext =
--       ChainSync.ClientStNext
--         { ChainSync.recvMsgRollForward = \header _pHead ->
--             ChainSync.ChainSyncClient $ do
--               addBlock header
--               cm <- atomically controlMessageSTM
--               return $ case cm of
--                 Terminate -> terminate
--                 _ -> requestNext,
--           ChainSync.recvMsgRollBackward = \pIntersect _pHead ->
--             ChainSync.ChainSyncClient $ do
--               rollback pIntersect
--               cm <- atomically controlMessageSTM
--               return $ case cm of
--                 Terminate -> terminate
--                 _ -> requestNext
--         }

--     addBlock :: BlockHeader -> IO ()
--     addBlock b = do
--       chain <- atomically $ do
--         chain <- readTVar candidateChainVar
--         let !chain' = shiftAnchoredFragment 50 b chain
--         writeTVar candidateChainVar chain'
--         return chain'
--       traceWith syncTracer (AF.lastPoint chain, AF.headPoint chain)

--     rollback :: Point BlockHeader -> IO ()
--     rollback p = atomically $ do
--       chain <- readTVar candidateChainVar
--       -- we do not handle rollback failure in this demo
--       let (Just !chain') = AF.rollback p chain
--       writeTVar candidateChainVar chain'

-- {-
-- notTooFarAhead = atomically $ do
--     currentChain   <- readTVar currentChainVar
--     candidateChain <- readTVar candidateChainVar
--     check $ case (AF.headBlockNo currentChain,
--                   AF.headBlockNo candidateChain) of
--               (Just bn, Just bn') -> bn' < bn + 5
--               _                   -> True
-- -}

codecChainSync
    :: ProtVer
    -> ShelleyGenesis
    -> Codec
        (ChainSync.ChainSync Header Point Tip)
        CBOR.DeserialiseFailure
        IO
        LBS.ByteString
codecChainSync _protVer _shelleyGenesis =
    ChainSync.codecChainSync
        enc
        dec
        CBOR.encode
        CBOR.decode
        encTip
        decTip
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


-- genesisAnchoredFragment :: AF.AnchoredFragment BlockHeader
-- genesisAnchoredFragment = AF.Empty AF.AnchorGenesis

-- shiftAnchoredFragment ::
--   (HasHeader block) =>
--   Int ->
--   block ->
--   AF.AnchoredFragment block ->
--   AF.AnchoredFragment block
-- shiftAnchoredFragment n b af =
--   AF.anchorNewest (fromIntegral (AF.length af - n)) af AF.:> b

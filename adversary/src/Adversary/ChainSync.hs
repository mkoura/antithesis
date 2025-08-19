{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Adversary.ChainSync where

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
import Control.Tracer (Contravariant (contramap), stdoutTracer)
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
    , Point
    , castPoint
    )
import Ouroboros.Network.Diffusion.Configuration
    ( PeerSharing (PeerSharingDisabled)
    )
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mock.Chain (Chain)
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
import Ouroboros.Network.Protocol.Handshake
    ( Acceptable (acceptableVersion)
    , Queryable (queryVersion)
    , cborTermVersionDataCodec
    , noTimeLimitsHandshake
    , nodeToNodeHandshakeCodec
    , simpleSingletonVersions
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

clientChainSync
    :: String
    -> PortNumber
    -> IO ()
clientChainSync peerName peerPort = withIOManager $ \iocp -> do
    AddrInfo{addrAddress} <- resolve
    var <- newTVarIO Chain.Genesis
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

    --    app :: OuroborosApplicationWithMinimalCtx Mx.InitiatorMode addr LBS.ByteString IO () Void
    app var = demoProtocol2
        $ InitiatorProtocolOnly
        $ mkMiniProtocolCbFromPeer
        $ \_ctx ->
            ( contramap show stdoutTracer
            , codecChainSync
            , ChainSync.chainSyncClientPeer (client var)
            )

    client var = chainSyncClient var (controlledClient _foo)

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
    :: forall header block tip m a
     . ( HasHeader header
       , HeaderHash header ~ HeaderHash block
       , MonadSTM m
       )
    => StrictTVar m (Chain header)
    -> Client header (Point block) tip m a
    -> ChainSyncClient header (Point block) tip m a
chainSyncClient chainvar client =
    ChainSyncClient
        $ either SendMsgDone initialise <$> getChainPoints
  where
    initialise
        :: ([Point block], Client header (Point block) tip m a)
        -> ClientStIdle header (Point block) tip m a
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

    requestNext
        :: Client header (Point block) tip m a
        -> ClientStIdle header (Point block) tip m a
    requestNext client' =
        SendMsgRequestNext
            -- We have the opportunity to do something when receiving
            -- MsgAwaitReply. In this example we don't take up that opportunity.
            (pure ())
            (handleNext client')

    handleNext
        :: Client header (Point block) tip m a
        -> ClientStNext header (Point block) tip m a
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

    getChainPoints
        :: m (Either a ([Point block], Client header (Point block) tip m a))
    getChainPoints = do
        pts <-
            Chain.selectPoints recentOffsets <$> readTVarIO chainvar
        choice <- points client (fmap castPoint pts)
        pure $ case choice of
            Left a -> Left a
            Right client' -> Right (fmap castPoint pts, client')

    addBlock :: header -> m ()
    addBlock b = atomically $ do
        chain <- readTVar chainvar
        let !chain' = Chain.addBlock b chain
        writeTVar chainvar chain'

    rollback :: Point block -> m ()
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
    :: ( CBOR.Serialise block
       , CBOR.Serialise point
       , CBOR.Serialise tip
       )
    => Codec
        (ChainSync.ChainSync block point tip)
        CBOR.DeserialiseFailure
        IO
        LBS.ByteString
codecChainSync =
    ChainSync.codecChainSync
        CBOR.encode
        CBOR.decode
        CBOR.encode
        CBOR.decode
        CBOR.encode
        CBOR.decode

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

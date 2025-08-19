{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Adversary.ChainSync where

import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List (maximumBy)
import Data.List.Infinite (Infinite ((:<)))
import Data.List.Infinite qualified as Inf
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)

import Control.Applicative (many)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception
import Control.Monad (when)
import Control.Monad.Class.MonadTime.SI (Time (..))
import Control.Tracer

import System.Directory
import System.Random (RandomGen, StdGen)
import System.Random qualified as Random

import Options.Applicative qualified as Opts

import Codec.Serialise qualified as CBOR

import Network.TypedProtocol.Codec

import Network.Mux qualified as Mx

import Ouroboros.Network.NodeToClient
import Ouroboros.Network.NodeToNode

import Ouroboros.Network.AnchoredFragment qualified as AF
import Ouroboros.Network.Block
import Ouroboros.Network.ControlMessage (continueForever)
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.Mock.ConcreteBlock
import Ouroboros.Network.Mux
import Ouroboros.Network.Point (WithOrigin (..))
import Ouroboros.Network.Snocket
import Ouroboros.Network.Socket

import Ouroboros.Network.Driver
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Protocol.Handshake.Unversioned

import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Codec qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Server qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync

import Ouroboros.Network.Protocol.BlockFetch.Codec qualified as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Server qualified as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch

import Ouroboros.Network.DeltaQ (defaultGSV)

import Ouroboros.Network.Server.Simple qualified as Server.Simple

clientChainSync :: [FilePath]
                -> Maybe SlotNo
                -> IO ()
clientChainSync sockPaths maxSlotNo = withIOManager $ \iocp ->
    forConcurrently_ (zip [0..] sockPaths) $ \(index, sockPath) -> do
      threadDelay (50000 * index)
      void $ connectToNode
        (localSnocket iocp)
        makeLocalBearer
        ConnectToArgs {
          ctaHandshakeCodec      = unversionedHandshakeCodec,
          ctaHandshakeTimeLimits = noTimeLimitsHandshake,
          ctaVersionDataCodec    = unversionedProtocolDataCodec,
          ctaConnectTracers      = nullNetworkConnectTracers,
          ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
        }
        mempty
        (simpleSingletonVersions
           UnversionedProtocol
           UnversionedProtocolData
           (\_ -> app))
        Nothing
        (localAddressFromPath sockPath)

  where
    app :: OuroborosApplicationWithMinimalCtx Mx.InitiatorMode addr LBS.ByteString IO () Void
    app = demoProtocol2 $
          InitiatorProtocolOnly $
          mkMiniProtocolCbFromPeer $ \_ctx ->
            ( contramap show stdoutTracer
            , codecChainSync
            , ChainSync.chainSyncClientPeer (chainSyncClient (continueForever Proxy) maxSlotNo)
            )

-- TODO: provide sensible limits
-- https://github.com/intersectmbo/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = maxBound
    }


--
-- Chain sync demo
--

demoProtocol2
  :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b -- ^ chainSync
  -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
demoProtocol2 chainSync =
    OuroborosApplication [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolStart  = StartOnDemand,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = chainSync
      }
    ]


chainSyncClient :: ControlMessageSTM IO
                -> Maybe SlotNo
                -> ChainSync.ChainSyncClient
                     BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
chainSyncClient controlMessageSTM maxSlotNo =
    ChainSync.ChainSyncClient $ do
      curvar   <- newTVarIO genesisAnchoredFragment
      chainvar <- newTVarIO genesisAnchoredFragment
      case chainSyncClient' controlMessageSTM maxSlotNo  nullTracer curvar chainvar of
        ChainSync.ChainSyncClient k -> k

chainSyncClient' :: ControlMessageSTM IO
                 -> Maybe SlotNo
                 -> Tracer IO (Point BlockHeader, Point BlockHeader)
                 -> StrictTVar IO (AF.AnchoredFragment BlockHeader)
                 -> StrictTVar IO (AF.AnchoredFragment BlockHeader)
                 -> ChainSync.ChainSyncClient
                      BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
chainSyncClient' controlMessageSTM _maxSlotNo syncTracer _currentChainVar candidateChainVar =
    ChainSync.ChainSyncClient (return requestNext)
  where
    requestNext :: ChainSync.ClientStIdle
                     BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
    requestNext =
      ChainSync.SendMsgRequestNext
        (pure ())   -- on MsgAwaitReply; could trace
        handleNext

    terminate :: ChainSync.ClientStIdle
                     BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
    terminate = ChainSync.SendMsgDone ()

    handleNext :: ChainSync.ClientStNext
                    BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
    handleNext =
      ChainSync.ClientStNext {
        ChainSync.recvMsgRollForward  = \header _pHead ->
          ChainSync.ChainSyncClient $ do
            addBlock header
            cm <- atomically controlMessageSTM
            return $ case cm of
              Terminate -> terminate
              _         -> requestNext

      , ChainSync.recvMsgRollBackward = \pIntersect _pHead ->
          ChainSync.ChainSyncClient $ do
            rollback pIntersect
            cm <- atomically controlMessageSTM
            return $ case cm of
              Terminate -> terminate
              _         -> requestNext
      }

    addBlock :: BlockHeader -> IO ()
    addBlock b = do
        chain <- atomically $ do
          chain <- readTVar candidateChainVar
          let !chain' = shiftAnchoredFragment 50 b chain
          writeTVar candidateChainVar chain'
          return chain'
        traceWith syncTracer (AF.lastPoint chain, AF.headPoint chain)

    rollback :: Point BlockHeader -> IO ()
    rollback p = atomically $ do
        chain <- readTVar candidateChainVar
        -- we do not handle rollback failure in this demo
        let (Just !chain') = AF.rollback p chain
        writeTVar candidateChainVar chain'
    {-
    notTooFarAhead = atomically $ do
        currentChain   <- readTVar currentChainVar
        candidateChain <- readTVar candidateChainVar
        check $ case (AF.headBlockNo currentChain,
                      AF.headBlockNo candidateChain) of
                  (Just bn, Just bn') -> bn' < bn + 5
                  _                   -> True
    -}



codecChainSync :: ( CBOR.Serialise block
                  , CBOR.Serialise point
                  , CBOR.Serialise tip
                  )
               => Codec (ChainSync.ChainSync block point tip)
                        CBOR.DeserialiseFailure
                        IO LBS.ByteString
codecChainSync =
    ChainSync.codecChainSync
      CBOR.encode CBOR.decode
      CBOR.encode CBOR.decode
      CBOR.encode CBOR.decode

genesisAnchoredFragment :: AF.AnchoredFragment BlockHeader
genesisAnchoredFragment = AF.Empty AF.AnchorGenesis

shiftAnchoredFragment :: HasHeader block
                      => Int
                      -> block
                      -> AF.AnchoredFragment block
                      -> AF.AnchoredFragment block
shiftAnchoredFragment n b af =
  AF.anchorNewest (fromIntegral (AF.length af - n)) af AF.:> b

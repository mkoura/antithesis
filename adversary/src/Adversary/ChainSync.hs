{-# LANGUAGE BangPatterns         #-}
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

import Cardano.Network.NodeToClient
import Cardano.Network.NodeToNode

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

import Ouroboros.Network.BlockFetch
import Ouroboros.Network.BlockFetch.Client
import Ouroboros.Network.BlockFetch.ClientRegistry (FetchClientRegistry (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (ChainSelStarvation (..),
           initialWithFingerprint)
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

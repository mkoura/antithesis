{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Adversary.ChainSync where

import Control.Concurrent.Class.MonadSTM.Strict

import Ouroboros.Network.Block (HasHeader (..), HeaderHash, Tip (..), castPoint,
           castTip, genesisPoint)
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Mock.Chain (Chain (..), ChainUpdate (..), Point (..))
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.Mock.ProducerState (ChainProducerState, FollowerId)
import Ouroboros.Network.Mock.ProducerState qualified as ChainProducerState
import Ouroboros.Network.Protocol.ChainSync.Client
import Ouroboros.Network.Protocol.ChainSync.Server

data Client header point tip m t = Client
  { rollbackward :: point -> tip -> m (Either t (Client header point tip m t))
  , rollforward  :: header -> m (Either t (Client header point tip m t))
  , points       :: [point] -> m (Either t (Client header point tip m t))
  }

-- | A client which doesn't do anything and never ends. Used with
-- 'chainSyncClientExample', the StrictTVar m (Chain header) will be updated but
-- nothing further will happen.
pureClient :: Applicative m => Client header point tip m void
pureClient = Client
  { rollbackward = \_ _ -> pure (Right pureClient)
  , rollforward  = \_ -> pure (Right pureClient)
  , points       = \_ -> pure (Right pureClient)
  }

controlledClient :: MonadSTM m
                 => ControlMessageSTM m
                 -> Client header point tip m ()
controlledClient controlMessageSTM = go
  where
    go = Client
      { rollbackward = \_ _ -> do
          ctrl <- atomically controlMessageSTM
          case ctrl of
            Continue  -> pure (Right go)
            Quiesce   -> error "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
            Terminate -> pure (Left ())
      , rollforward = \_ -> do
          ctrl <- atomically controlMessageSTM
          case ctrl of
            Continue  -> pure (Right go)
            Quiesce   -> error "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
            Terminate -> pure (Left ())
      , points = \_ -> pure (Right go)
      }


-- | An instance of the client side of the chain sync protocol that
-- consumes into a 'Chain' stored in a 'StrictTVar'.
--
-- This is of course only useful in tests and reference implementations since
-- this is not a realistic chain representation.
--
chainSyncClientExample :: forall header block tip m a.
                          ( HasHeader header
                          , HasHeader block
                          , HeaderHash header ~ HeaderHash block
                          , MonadSTM m
                          )
                       => StrictTVar m (Chain header)
                       -> Client header (Point block) tip m a
                       -> ChainSyncClient header (Point block) tip m a
chainSyncClientExample chainvar client = ChainSyncClient $
    either SendMsgDone initialise <$> getChainPoints
  where
    initialise :: ([Point block], Client header (Point block) tip m a)
               -> ClientStIdle header (Point block) tip m a
    initialise (points, client') =
      SendMsgFindIntersect points $
      -- In this consumer example, we do not care about whether the server
      -- found an intersection or not. If not, we'll just sync from genesis.
      --
      -- Alternative policies here include:
      --  iteratively finding the best intersection
      --  rejecting the server if there is no intersection in the last K blocks
      --
      ClientStIntersect {
        recvMsgIntersectFound    = \_ _ -> ChainSyncClient (return (requestNext client')),
        recvMsgIntersectNotFound = \  _ -> ChainSyncClient (return (requestNext client'))
      }

    requestNext :: Client header (Point block) tip m a
                -> ClientStIdle header (Point block) tip m a
    requestNext client' =
      SendMsgRequestNext
        -- We have the opportunity to do something when receiving
        -- MsgAwaitReply. In this example we don't take up that opportunity.
        (pure ())
        (handleNext client')

    handleNext :: Client header (Point block) tip m a
               -> ClientStNext header (Point block) tip m a
    handleNext client' =
      ClientStNext {
        recvMsgRollForward  = \header _tip -> ChainSyncClient $ do
          addBlock header
          choice <- rollforward client' header
          pure $ case choice of
            Left a         -> SendMsgDone a
            Right client'' -> requestNext client''

      , recvMsgRollBackward = \pIntersect tip -> ChainSyncClient $ do
          rollback pIntersect
          choice <- rollbackward client' pIntersect tip
          pure $ case choice of
            Left a         -> SendMsgDone a
            Right client'' -> requestNext client''
      }

    getChainPoints :: m (Either a ([Point block], Client header (Point block) tip m a))
    getChainPoints = do
      pts <- Chain.selectPoints recentOffsets <$> atomically (readTVar chainvar)
      choice <- points client (fmap castPoint pts)
      pure $ case choice of
        Left a        -> Left a
        Right client' -> Right (fmap castPoint pts, client')

    addBlock :: header -> m ()
    addBlock b = atomically $ do
        chain <- readTVar chainvar
        let !chain' = Chain.addBlock b chain
        writeTVar chainvar chain'

    rollback :: Point block -> m ()
    rollback p = atomically $ do
        chain <- readTVar chainvar
        --TODO: handle rollback failure
        let !chain' = case Chain.rollback (castPoint p) chain of
              Just a  -> a
              Nothing -> error "out of scope rollback"
        writeTVar chainvar chain'

-- | Offsets from the head of the chain to select points on the consumer's
-- chain to send to the producer. The specific choice here is fibonacci up
-- to 2160.
--
recentOffsets :: [Int]
recentOffsets = [0,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584]

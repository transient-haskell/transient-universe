-----------------------------------------------------------------------------
--
-- Module      :  Transient.Move
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | @transient-universe@ extends the seamless composability of concurrent
-- multi-threaded programs provided by
-- <https://github.com/transient-haskell/transient transient>
-- to a multi-node cloud.  Distributed concurrent programs are created and
-- composed seamlessly and effortlessly as if they were written for a single
-- node.  @transient-universe@ has diverse applications from simple distributed
-- applications to massively parallel and distributed map-reduce problems.  If
-- you are considering Apache Spark or Cloud Haskell then transient might be a
-- simpler yet better solution for you.
--
-- Transient makes it easy to write composable, distributed event driven
-- reactive UI applications with client side and server side code composed
-- freely in the same application. For example,
-- <https://hackage.haskell.org/package/axiom Axiom> is a transient based
-- unified client and server side web application framework that provides a
-- better programming model and composability compared to frameworks like
-- ReactJS.
--
-- = Overview
--
-- The 'Cloud' monad adds the following facilities to complement the 'TransIO'
-- monad:
--
-- * Create a distributed compute cluster of nodes
-- * Move computations across nodes at any point during computation
-- * Run computations on multiple nodes in parallel
--
-- = Further Reading
--
-- * <https://github.com/transient-haskell/transient/wiki/Transient-tutorial Tutorial>
-- * <https://github.com/transient-haskell/transient-examples Examples>
-- * <https://www.fpcomplete.com/user/agocorona/moving-haskell-processes-between-nodes-transient-effects-iv Blog post>
--
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Transient.Move(

-- * Running the Monad
Cloud(..),runCloud, runCloudIO, runCloudIO',

-- * Node Management
createNode, createWebNode, createNodeServ, getMyNode, getNodes,
addNodes, shuffleNodes,

-- * Connecting
listen, Transient.Move.Internals.connect, connect',

-- * Running Local Computations
local, onAll, lazy, loggedc, lliftIO, localIO, fullStop,

-- * Moving Computations
wormhole, teleport, copyData,

-- * Running at a Remote Node
beamTo, forkTo, callTo, runAt, atRemote,

-- * Running at Multiple Nodes
clustered, mclustered, callNodes,

-- * Messaging
putMailbox, putMailbox',getMailbox,getMailbox',cleanMailbox,cleanMailbox',

-- * Thread Control
single, unique,

#ifndef ghcjs_HOST_OS
-- * Buffering Control
setBuffSize, getBuffSize,
#endif

#ifndef ghcjs_HOST_OS
-- * REST API
api, HTTPMethod(..), PostParams,
#endif

-- * Low Level APIs

 getWebServerNode, Node(..), nodeList, Connection(..), Service(),
 isBrowserInstance,

 defConnection,
 ConnectionData(..),
#ifndef ghcjs_HOST_OS
 ParseContext(..)
#endif

) where

import Transient.Move.Internals







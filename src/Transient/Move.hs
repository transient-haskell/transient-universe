-----------------------------------------------------------------------------
--
-- Module      :  Transient.Move
-- Copyright   :
-- License     :  GPL-3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | see <https://www.fpcomplete.com/user/agocorona/moving-haskell-processes-between-nodes-transient-effects-iv>
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Transient.Move(

-- * running the Cloud monad
Cloud(..),runCloud, runCloudIO, runCloudIO', local, onAll, lazy, loggedc, lliftIO,localIO,
listen, Transient.Move.Internals.connect, connect', fullStop,

-- * primitives for communication
wormhole, teleport, copyData,



-- * single node invocation
beamTo, forkTo, callTo, runAt, atRemote,

-- * invocation of many nodes
clustered, mclustered, callNodes,

-- * messaging
putMailbox, putMailbox',getMailbox,getMailbox',cleanMailbox,cleanMailbox',

-- * thread control
single, unique,

#ifndef ghcjs_HOST_OS
-- * buffering control
setBuffSize, getBuffSize,
#endif

#ifndef ghcjs_HOST_OS
-- * REST API
api, HTTPMethod(..), PostParams,
#endif
-- * node management
createNode, createWebNode, createNodeServ, getMyNode, getNodes,
addNodes, shuffleNodes,

-- * low level

 getWebServerNode, Node(..), nodeList, Connection(..), Service(),
 isBrowserInstance,

 defConnection,
 ConnectionData(..),
#ifndef ghcjs_HOST_OS
 ParseContext(..)
#endif

) where

import Transient.Move.Internals







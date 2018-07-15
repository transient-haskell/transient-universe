-----------------------------------------------------------------------------
--
-- Module      :  Transient.Move.Services
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables, CPP, FlexibleInstances, UndecidableInstances #-}

#ifndef ghcjs_HOST_OS

module Transient.Move.Services  where

import Transient.Internals
import Transient.Move.Internals
-- import Transient.Backtrack
-- import Transient.Internals(RemoteStatus(..), Log(..))
import Transient.Move.Utils

import Control.Monad.IO.Class
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Applicative
import System.Process
import Control.Concurrent(threadDelay)
import Control.Exception hiding(onException)
import Data.IORef

monitorService= [("service","monitor")
                ,("executable", "monitorService")
                ,("package","https://github.com/transient-haskell/transient-universe")]


monitorPort= 3000

-- | initService search for a node in the list of nodes that the local node may know, instead of calling
-- the monitor. if there is no such node, it call requestInstance. `initService` is used by `callService`
initService :: String -> Service -> Cloud Node
initService ident service=
    cached <|> installIt
    where
    cached= local $ do
        ns <- findInNodes service 
        if null ns then  empty
        else return $ head ns
    installIt= do
        ns <- requestInstance ident service 1 
        if null ns then empty else return $ head ns

-- |  receives the specification of a service and install (if necessary) and run it (if necessary)
--    if the service is running, it return the node immediately.
-- if the monitor service executable is not running `requestInstace` initiates it.
-- Instances are provisioned  among the available nodes
-- The returned nodes are added to the list of known nodes.

requestInstance :: String -> Service -> Int -> Cloud [Node]
requestInstance ident service num=  loggedc $ do
    --    return () !> "requestInstance"
       local $ onException $ \(e:: ConnectionError) ->  startMonitor >> continue   --   !> ("EXCEPTIOOOOOOOOOOON",e)
       nodes <- callService' ident monitorNode (ident,service,num)
       local $ addNodes nodes      -- !> ("ADDNODES",service)
       return nodes

startMonitor :: MonadIO m => m ()
startMonitor=  liftIO $ do
        (_,_,_,h) <- createProcess . shell $ "monitorService -p start/localhost/"++ show monitorPort ++ " > monitor.log 2>&1"
        writeIORef monitorHandle $ Just h
        threadDelay 2000000

monitorHandle= unsafePerformIO $ newIORef Nothing

endMonitor= do
    mm <- readIORef monitorHandle
    case mm of
        Nothing -> return ()  
        Just h  -> interruptProcessGroupOf h

findInNodes :: Service -> TransIO [Node]
findInNodes service =  do
      return () !> "FINDINNODES"
      nodes <-  getNodes
      return $ filter (\node  -> head service == head  (nodeServices node)) nodes
     



rfriends        =   unsafePerformIO $ newIORef ([] ::[String])
rservices       =   unsafePerformIO $ newIORef ([] ::[Service])
ridentsBanned   =   unsafePerformIO $ newIORef ([] ::[String])
rServicesBanned =   unsafePerformIO $ newIORef ([] ::[Service])

inputAuthorizations= do
    oneThread $ option "auth" "add authorizations for users and services"
    showPerm <|> friends <|> services <|> identBanned <|> servicesBanned
    empty

    where
    friends= do
      option "friends" "friendsss"
      fr <- input (const True) "enter the friend list: "
      liftIO $ writeIORef rfriends (fr :: [String])

    services= do
      option "services" "services"
      serv <- input (const True) "enter service list: "
      liftIO $ writeIORef rservices (serv :: [Service])

    identBanned= do
      option "bannedIds"  "banned users"
      ban <- input (const True) "enter the users banned: "
      liftIO $ writeIORef ridentsBanned (ban ::[String ])
      rs <- liftIO $ readIORef ridentsBanned
      liftIO $ print rs

    servicesBanned= do
      option "bannedServ"  "banned services"
      ban <- input (const True) "enter the services banned: "
      liftIO $ writeIORef rServicesBanned (ban :: [Service])

    showPerm= do
     option "show"  "show permissions"
     friends            <- liftIO $ readIORef rfriends
     services           <- liftIO $ readIORef rservices
     identsBanned       <- liftIO $ readIORef ridentsBanned
     servicesBanned     <- liftIO $ readIORef rServicesBanned
     liftIO $ putStr "allowed:          " >> print friends
     liftIO $ putStr "banned:           " >> print identsBanned
     liftIO $ putStr "services allowed: " >> print services
     liftIO $ putStr "services banned:  " >> print servicesBanned

rfreePort :: MVar Int
rfreePort = unsafePerformIO $ newMVar  (monitorPort +1)

freePort :: MonadIO m => m Int
freePort= liftIO $ modifyMVar rfreePort $ \ n -> return (n+1,n)


authorizeService :: MonadIO m => String -> Service -> m Bool
authorizeService ident service=   do

     friends            <- liftIO $ readIORef rfriends
     services           <- liftIO $ readIORef rservices
     identsBanned       <- liftIO $ readIORef ridentsBanned
     servicesBanned     <- liftIO $ readIORef rServicesBanned

     return $ if (null friends || ident `elem` friends)
        && (null services || service `elem` services)
        && (null identsBanned || ident `notElem` identsBanned)
        && (null servicesBanned || service `notElem` servicesBanned)
      then True  else False
  where
  notElem a b= not $ elem a b

-- | call a service. If the service is not running in some node, the monitor service would install
-- and run it. The first parameter is a weak password.
callService
    :: (Loggable a, Loggable b)
    => String -> Service -> a  -> Cloud b
callService ident service params = do
    node <-  initService ident service       -- !> ("callservice initservice", service)
    callService' ident node params           -- !>  ("NODE FOR SERVICE",node)

monitorNode= unsafePerformIO $ createNodeServ "localhost"
            (fromIntegral monitorPort)
            monitorService

callService' ident node params = do
    log <- onAll $ do
             log  <- getSData <|> return emptyLog
             setData emptyLog  
             return log

    r <- wormhole node $  do
             local $ return params
             
             teleport
          --   local empty  `asTypeOf` typea params
             local empty

    restoreLog log                        --  !> "RESTORELOG"

    return  r
    where
    typea :: a -> Cloud a
    typea = undefined
    restoreLog (Log _ _ logw hash)= onAll $ do
       Log _ _ logw' hash' <- getSData <|> return emptyLog

       let newlog= reverse logw' ++ logw
--       return ()                 !> ("newlog", logw,logw')
       setData $ Log False newlog newlog (hash + hash')

    emptyLog= Log False [] [] 0

-- catchc :: Exception e => Cloud a -> (e -> Cloud a) -> Cloud a
-- catchc a b= Cloud $ catcht (runCloud' a) (\e -> runCloud' $ b e)

runEmbeddedService :: (Loggable a, Loggable b) =>  Service -> (a -> Cloud b) -> Cloud b
runEmbeddedService servname serv =  do
   node <- localIO $ do
          port <- freePort
          createNodeServ "localhost" (fromIntegral port) servname
   listen node
   wormhole (notused 4) $ loggedc $ do
      x <- local $ return (notused 0)
      r <- onAll $ runCloud (serv x) <** setData WasRemote
      local $ return r
      teleport
      return r

  

notused n= error $  "runService: "++ show (n::Int) ++ " variable should not be used"

runService :: (Loggable a, Loggable b) =>  Service -> Int -> (a -> Cloud b) -> Cloud b
runService servname defPort serv =  do
   onAll $ onException $ \(e :: SomeException)->  liftIO $ print e 
   initNodeServ servname
   service 
   where
   service=
       wormhole (notused 1) $  do
          x <- local . return $ notused 2
          r <- local $ runCloud  (serv x) -- <** setData WasRemote
          setData emptyLog
          local $ return r
          teleport
          return r

   emptyLog= Log False [] [] 0

   initNodeServ  servs=do
      mynode <- local  getNode

      local $ do
         conn <- defConnection
         liftIO $ writeIORef (myNode conn) mynode
         setState conn
      onAll inputAuthorizations <|> (inputNodes >> empty) <|> return ()
      listen mynode  

      where
      getNode :: TransIO Node
      getNode =  if isBrowserInstance then liftIO createWebNode else do
          oneThread $ option "start" "re/start node"
          host <- input' (Just "localhost") (const True) "hostname of this node (must be reachable) (\"localhost\"): "
          port <- input' (Just 3000) (const True)  "port to listen? (3000) "
          liftIO $ createNodeServ host port servs

      inputNodes= do
           onServer $ do
                  local $ option "add"  "add a new monitor node"

                  host <- local $ do
                            r <- input (const True) "Host to connect to: (none): "
                            if r ==  "" then stop else return r

                  port <-  local $ input (const True) "port? "

                  nnode <- localIO $ createNodeServ host port monitorService
                  local $ do
                       liftIO $ putStr "Added node: ">> print nnode
                       addNodes [nnode]
           empty
#else
requestInstance :: String -> Service -> Int -> Cloud [Node]
requestInstance ident service num= logged empty
#endif



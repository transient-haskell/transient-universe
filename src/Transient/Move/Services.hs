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
import Transient.Logged
import Transient.Move.Internals
-- import Transient.Backtrack
-- import Transient.Internals(RemoteStatus(..), Log(..))
import Transient.Move.Utils

import Control.Monad.State
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Applicative
import System.Process
import Control.Concurrent(threadDelay)
import Control.Exception hiding(onException)
import Data.IORef
import Control.Monad(when) 
import Data.Typeable
import System.Random
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
monitorService= [("service","monitor")
                ,("executable", "monitorService")
                ,("package","https://github.com/transient-haskell/transient-universe")]


monitorPort= 3000

reInitService :: String -> Node -> Cloud Node
reInitService  ident node= loggedc $
  cached <|> installIt
    where
    cached= local $ do
        ns <- findInNodes $ nodeServices node 

        if null ns then  empty
         else do
          ind <- liftIO $ randomRIO(0,length ns-1)
          return $  ns !! ind
    installIt= do                               -- TODO block by service name, to avoid  double initializations
        ns <- requestInstanceFail ident node 1 
        if null ns then empty else return $ head ns
        
        
-- | initService search for any node in the list of nodes that the local node may know, for that service, instead of calling
-- the monitor. if there is no such node, it request an instance from the monitor `requestInstance`. `initService` is used by `callService`
initService :: String -> Service -> Cloud Node
initService ident service= loggedc $
 cached <|> installIt
    where
    cached= local $ do
        ns <- findInNodes service 
        if null ns then  empty
         else do
          ind <- liftIO $ randomRIO(0,length ns-1)
          return $  ns !! ind
    installIt= do                               -- TODO block by service name, to avoid  double initializations
        ns <- requestInstance ident service 1 
        if null ns then empty else return $ head ns

-- |  receives the specification of a service and install (if necessary) and run it (if necessary)
--    if the service has been started previously, it returns the node immediately.
-- if the monitor service executable is not running `requestInstace` initiates it.
-- Instances are provisioned  among the available nodes
-- The returned nodes are added to the list of known nodes.

requestInstance :: String -> Service -> Int -> Cloud [Node]
requestInstance ident service num=  loggedc $ do
       local $ onException $ \(e:: ConnectionError) -> do
                  liftIO $ putStrLn "Monitor was not running. STARTING MONITOR"
                  startMonitor
                  
                  continue  
       
       nodes <- callService' ident monitorNode (ident,service, num )
       local $ addNodes nodes                                                       -- !> ("ADDNODES",service)
       return nodes

requestInstanceFail :: String -> Node -> Int -> Cloud [Node]
requestInstanceFail ident node num=  loggedc $ do
       return () !> "REQUEST INSTANCEFAIL"
       local $ delNodes [node]
       local $ onException $ \(e:: ConnectionError) ->  startMonitor >> continue      !> ("EXCEPTIOOOOOOOOOOON",e)
       
       nodes <- callService' ident monitorNode (ident,node, num )                    !> "CALLSERVICE'"
       local $ addNodes nodes                                                        !> ("ADDNODES")
       return nodes


sendStatusToMonitor :: String -> String -> Cloud ()
sendStatusToMonitor ident status= loggedc $ do
       local $ onException $ \(e:: ConnectionError) ->  startMonitor >> continue    -- !> ("EXCEPTIOOOOOOOOOOON",e)
       nod <- local getMyNode
       callService'  ident monitorNode (nodePort nod, status) -- <|> return()

rmonitor= unsafePerformIO $ newMVar ()  -- to avoid races starting the monitor
startMonitor :: MonadIO m => m ()
startMonitor = liftIO $ do
    return () !> "START MONITOR"
    b <- tryTakeMVar rmonitor
    when (b== Just()) $ do
        (_,_,_,h) <- createProcess . shell $ "monitorService -p start/localhost/"++ show monitorPort ++ " > monitor.log 2>&1"
        writeIORef monitorHandle $ Just h
        putMVar rmonitor ()
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

      return $ filter (\node  -> head service == head1  (nodeServices node)) nodes
     
      where
      head1 []= ("","")
      head1 x= head x


rfriends        =   unsafePerformIO $ newIORef ([] ::[String])
rservices       =   unsafePerformIO $ newIORef ([] ::[Service])
ridentsBanned   =   unsafePerformIO $ newIORef ([] ::[String])
rServicesBanned =   unsafePerformIO $ newIORef ([] ::[Service])

inputAuthorizations= local $ do
    abduce
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
freePort=   liftIO $ modifyMVar rfreePort $ \ n -> return (n+1,n)


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
    node <-  initService ident service        !> ("callservice initservice", service)
    callService' ident node params            !>  ("NODE FOR SERVICE",node)

-- | notify the the monitor that a node has failed for a service and reclaim another
-- to execute the request. If the service is not running in some node, the monitor service would install
-- and run it. The first parameter is a weak password.
callServiceFail
    :: (Loggable a, Loggable b)
    => String -> Node -> a  -> Cloud b
callServiceFail ident node params = do
    node <- reInitService ident node     
    callService' ident node params


monitorNode= unsafePerformIO $ createNodeServ "localhost"
            (fromIntegral monitorPort)
            monitorService


-- | call a service located in a node
callService' :: (Loggable a, Loggable b) => String -> Node -> a -> Cloud b 
callService' ident node params = r
 where 
 r= loggedc $ do
    onAll abduce
             
    service 
    
    where
    service = do

      localFixServ True
     
      mr <- wormhole node $  do

             local $ return $ toIDyn params

             teleport

             local empty

      local $ delData  ( undefined ::  (Bool,Int ,Int ,IORef (M.Map Int Int)))
      case maybeFromIDyn mr  of
        Just x  -> return x
        Nothing -> error $ "type mismatch calling service (data,input type,expected return type,node/service)= "
             ++ show (mr,typeOf params, typeOf(typeof1 r), node) 
 typeof1 :: Cloud b -> b
 typeof1= error "typeof: type level"
 
          
                    
    -- on exception, callService is called to reclaim a new node to the monitor if necessary
    
 ---- `catchc` \(e :: SomeException ) -> do onAll $ delNodes [node] ; callServiceFail ident  node params
    
    
 typea :: a -> Cloud a
 typea = undefined
 restoreLog (Log _ _ logw hash)= onAll $ do
       Log _ _ logw' hash' <- getSData <|> return emptyLog

       let newlog= reverse logw' ++ logw
--       return ()                 !> ("newlog", logw,logw')
       setData $ Log False newlog newlog (hash + hash')
emptyLog= Log False [] [] 0

catchc :: Exception e => Cloud a -> (e -> Cloud a) -> Cloud a
catchc a b= Cloud $ catcht (runCloud' a) (\e -> runCloud' $ b e)

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

-- | executes a program that export a service/services.
-- It receives the service description, a default port, the services to set up and the computation to start.
-- for example the monitor exposes two services, and is started with:
--
-- >  main = keep . runCloud $ runService monitorService 3000 $
-- >                       [serve returnInstances
-- >                       ,serve addToLog] pings
--
-- every service executable incorporates a ping service and a error service, invoqued when the parameter received
-- do not match with any of the endpoints implemented.
runService :: Service -> Int -> [(IDynamic -> Cloud IDynamic)] -> Cloud () -> Cloud ( IDynamic)
runService servDesc defPort servs proc= runService' servDesc defPort serv1  proc
   where 
   --serv1 :: IDynamic -> Cloud IDynamic
   serv1 d =  foldr  (<|>) empty $ map (\f ->  f d) $ servs ++ [serve ping ,serve serveerror]
   --init :: String -> Cloud ()
   --init= const proc
   ping :: () -> Cloud ()
   ping = const $ return() !> "PING"
   

   
   serveerror d= localIO $  
       print $ "parameter mismatch calling service  (parameter,service): "++ show (d :: IDynamic,servDesc)



   runService' :: Service -> Int -> (IDynamic -> Cloud IDynamic) -> Cloud() -> Cloud (  IDynamic)
   runService' servDesc defPort serv proc=  do
       onAll $ onException $ \(e :: SomeException) -> liftIO $ print e

       initNodeServ servDesc 
       return () !> "INITTTTTT"
       services  

       where
     {-  mainProc  = loggedc $ do

          proc
          empty
         <** setState WasRemote
         -} 
          
       services= do
          onAll abduce
          wormhole (notused 1) $  do
              x <- local . return $ notused 2
    
              r <- local $  runCloud' $  serv1 x --  <** setData WasRemote
              return () !> ("SENDING",r)
              setData emptyLog
              local $ return r
              teleport
              
              return r
          where
          serv1 x= serv x
              `catchc` \(e:: SomeException ) -> do
                   return () !> ("ERRORRRRRR:",e)
                   node <- local getMyNode
                   sendStatusToMonitor "" $ show e
                   local $ do
                      Closure closRemote <- getData `onNothing` error "teleport: no closRemote"
                      conn <- getData `onNothing` error "reportBack: No connection defined: use wormhole"
                      msend conn  $ SError $ toException $ ErrorCall $ show $ show $ CloudException node closRemote $ show e
                      empty


       emptyLog= Log False [] [] 0

       initNodeServ  servs=do
          mynode <- local  getNode

          local $ do
             conn <- defConnection
             liftIO $ writeIORef (myNode conn) mynode
             
             setState conn
             
          inputAuthorizations <|> (inputNodes >> empty) <|> return ()
          listen mynode 
    
          where
          getNode :: TransIO Node
          getNode =  if isBrowserInstance then liftIO createWebNode else do
              oneThread $ option "start" "re/start node"
              host <- input' (Just "localhost") (const True) "hostname of this node (must be reachable) (\"localhost\"): "
              port <- input' (Just 3000) (const True)  "port to listen? (3000) "
              liftIO $ createNodeServ host port servs
    
          inputNodes= onServer $ local $ do
                      abduce
                      
                      option "add"  "add a new service node"
    
                      host <- do
                                r <- input (const True) "Host to connect to: (none): "
                                if r ==  "" then stop else return r
    
                      port <-  input (const True) "port? "
    
                      nnode <- liftIO $ createNodeServ host port monitorService

                      liftIO $ putStr "Added node: ">> print nnode
                      addNodes [nnode]
                      empty
                      return()

-- | ping a service in a node. since services now try in other nodes created by the monitor until  succees, ping can be
-- used to preemptively assure that there is a node ready for the service.
ping node= callService' "" node ()  :: Cloud ()

-- | encode and decode parameters from/to the individual services. a service within a program is invoked if the types of
-- the parameters received match with what the service expect. See `runService` for a usage example
serve :: (Loggable a, Loggable b) => (a -> Cloud b) -> IDynamic ->   Cloud IDynamic
serve f d = do
       return () !> ("MAYBEFROMIDYN", typeOf f,d)

       case maybeFromIDyn d of
         Nothing -> empty
         Just x  -> toIDyn <$> f x --  (f x <** setState WasRemote)

executorService = [("service","executor")
                  ,("executable", "executor")
                  ,("package","https://github.com/transient-haskell/transient-universe")]



-- initialize N instances, of the executor service. The monitor would spread them among the nodes available.
-- the number N should be less of equal than the number of phisical machines.
-- Since the executor serivice can execute any number of processes, it sould be at most one per machine. 
initExecute ident number=    requestInstance  ident executorService  number

-- | execute a command in some node by an executor service, and return the result when the program finishes
networkExecute :: String -> String -> String ->  Cloud String
networkExecute ident cmdline input= 
     callService ident executorService (cmdline, input,())
     

-- | execute a shell command in some node using the executor service. 
-- The response is received as an stream of responses, one per line

networkExecuteStream :: String -> String -> Cloud String      -- '[Multithreaded,Streaming]
networkExecuteStream ident cmdline= do

    -- callService ident executorService cmdline
     node <- initService ident executorService
     localIO $ atomicModifyIORef rnodecmd $ \map -> (M.insert cmdline node map,())
     return () !> ("STORED NODE", node)
     callService' ident node cmdline

rnodecmd= unsafePerformIO $ newIORef M.empty

-- | send a message that will be read by the standard input of the program initiated by `networkExecuteStream`, identified by the command line.
-- the stream of responses is returned by that primitive. `sendExecuteStream` never return anything, since it is asynchronous
sendExecuteStream :: String -> String -> String -> Cloud  ()  -- '[Asynchronous]
sendExecuteStream ident cmdline msg=  do
     -- callService ident executorService (cmdline, msg) 
     return () !> ("SENDEXECUTE", cmdline)
     node <- localIO $ do
       map <- readIORef rnodecmd 
       let mn = M.lookup cmdline map 
       case mn of
         Nothing ->  error $ "sendExecuteStream: no node executing the command: "++ cmdline
         Just n -> return n
     return () !> ("NODE", node)
     callService' ident node (cmdline, msg)
     
     

#else
requestInstance :: String -> Service -> Int -> Cloud [Node]
requestInstance ident service num= logged empty
#endif




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
{-# LANGUAGE ScopedTypeVariables, CPP, FlexibleInstances
   , FlexibleContexts, UndecidableInstances, RecordWildCards
   , MultiParamTypeClasses, ExistentialQuantification #-}

{-
TODO:
 service=[("runsource", "this")]
 
    send the execution arguments, the source code to all monitors
    compile it using the command arguments
    find the host:port and set up them for each node

generate a web interface for each service:
   get the type of the argument
   parse the type and generate axiom source code.
-}



module Transient.Move.Services(
runService,callService, callService',callServiceFail,serve,ping
, monitorNode, monitorService, setRemoteJob,killRemoteJob

#ifndef ghcjs_HOST_OS

,initService,authorizeService,requestInstance,requestInstanceFail,requestInstanceHost
,findInNodes,endMonitor,freePort, controlNodeService, controlNode

-- * implementation details
,GetNodes(..)
,GetLog (..)
,ReceiveFromNodeStandardOutput (..)
,controlToken
#endif
)  where 

import Transient.Internals
import Transient.Logged
import Transient.Parse
import Transient.Move.Internals
import Transient.Move.Utils

import Control.Monad.State
import System.IO (hFlush,stdout) 
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Applicative

import Control.Concurrent(threadDelay)
import Control.Exception hiding(onException)
import Data.IORef
import Control.Monad(when) 
import Data.Typeable
import System.Random
import Data.Maybe 
import qualified Data.Map as M
import System.Environment
import Data.List(isPrefixOf)
import Unsafe.Coerce
import Data.Monoid 
import Data.String
import qualified Data.ByteString.Char8 as BSS
import qualified Data.ByteString.Lazy.Char8 as BS

#ifndef ghcjs_HOST_OS
import System.Directory
import Data.ByteString.Builder
import Transient.Parse
import GHC.IO.Handle
#else
import qualified Data.JSString as JS 
#endif



#ifndef ghcjs_HOST_OS
import System.Process
#endif 



monitorService= [("service","monitor")
                ,("executable", "monitorService")
                ,("package","https://github.com/transient-haskell/transient-universe")]


monitorPort= 3000

#ifndef ghcjs_HOST_OS

reInitService ::  Node -> Cloud Node
reInitService   node= loggedc $ cached <|> installIt
    where
    cached= local $ do
        ns <- findInNodes $ nodeServices node 
        if null ns then  empty
         else do
          ind <- liftIO $ randomRIO(0,length ns-1)
          return $  ns !! ind
    installIt= do                               -- TODO block by service name, to avoid  double initializations
        ns <- requestInstanceFail  node 1 
        if null ns then empty else return $ head ns
        
        
-- | initService search for any node in the list of nodes that the local node may know, for that service, instead of calling
-- the monitor. if there is no such node, it request an instance from the monitor `requestInstance`. `initService` is used by `callService`
initService :: Service -> Cloud Node
initService  service= loggedc $ cached  <|> installed  <|>  installIt
    where
    installed= local $ do
         --if has host-port key it has been installed manually
         host <- emptyIfNothing $ lookup "nodehost" service
         port <- emptyIfNothing $ lookup "nodeport" service
         node <- liftIO $ createNodeServ host (read' port) service
         addNodes [node]
         return node
         
    cached= local $ do
        ns <- findInNodes service
        if null ns then  empty
         else do
          ind <- liftIO $ randomRIO(0,length ns-1)
          return $  ns !! ind
          
    installIt= do                               -- TODO block by service name, to avoid  double initializations
        ns <- requestInstance  service 1 
        if null ns then empty else return $ head ns

-- |  receives the specification of a service and install (if necessary) and run it (if necessary)
--    if the servi ce has been started previously, it returns the node immediately.
-- if the monitor service executable is not running `requestInstace` initiates it.
-- Instances are provisioned  among the available nodes
-- The returned nodes are added to the list of known nodes.

requestInstance :: Service -> Int -> Cloud [Node]
requestInstance service num=  loggedc $ do
       local $ onException $ \(e:: ConnectionError) -> do
                  liftIO $ putStrLn $ show ("Monitor was not running. STARTING MONITOR for this machine",e)
                  continue
                  startMonitor
                  

       nodes <- callService'  monitorNode ("",service, num )
       local $ addNodes nodes                                                       -- !> ("ADDNODES",service)
       return nodes
       
requestInstanceHost :: String -> Service -> Cloud Node
requestInstanceHost hostname service= do
    monitorHost <- localIO $ createNodeServ hostname
            (fromIntegral monitorPort)
            monitorService
            

    nodes@[node] <- callService'  monitorHost  ("",service, 1::Int)
    local $ addNodes nodes
    return node

requestInstanceFail :: Node -> Int -> Cloud [Node]
requestInstanceFail node num=  loggedc $ do
       return () !> "REQUEST INSTANCEFAIL"
       local $ delNodes [node]
       local $ onException $ \(e:: ConnectionError) ->  do
           liftIO $ putStrLn "Monitor was not running. STARTING MONITOR"
           continue
           startMonitor                                                        !> ("EXCEPTIOOOOOOOOOOON",e)
       

       nodes <- callService' monitorNode ("", node, num )                      !> "CALLSERVICE'"
       local $ addNodes nodes                                                  !> ("ADDNODES")
       return nodes


rmonitor= unsafePerformIO $ newMVar ()  -- to avoid races starting the monitor
startMonitor :: TransIO () 
startMonitor = ( liftIO $ do
    return () !> "START MONITOR"
    b <- tryTakeMVar rmonitor
    when (b== Just()) $ do

        r <- findExecutable "monitorService"
        when ( r == Nothing) $ error "monitor not found"
        (_,_,_,h) <- createProcess $ (shell $ "monitorService -p start/localhost/"++ show monitorPort ++ " > monitor.log 2>&1"){std_in=NoStream}

        writeIORef monitorHandle $ Just h
        putMVar rmonitor ()

    threadDelay 2000000) 
  `catcht` \(e :: SomeException) -> do
        liftIO $ putStrLn "'monitorService' binary should be in some folder included in the $PATH variable. Computation aborted"
        empty
        
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

inputAuthorizations :: Cloud ()
inputAuthorizations=  onServer $ Cloud $ do
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
rfreePort = unsafePerformIO $ newMVar  (monitorPort +2) -- executor use 3001 by default

freePort :: MonadIO m => m Int
freePort=   liftIO $ modifyMVar rfreePort $ \ n -> return (n+1,n)



authorizeService :: MonadIO m =>  String -> Service -> m Bool
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


runEmbeddedService :: (Loggable a, Loggable b) =>  Service -> (a -> Cloud b) -> Cloud b
runEmbeddedService servname serv =  do
   node <- localIO $ do
          port <- freePort
          createNodeServ "localhost" (fromIntegral port) servname
   listen node
   wormhole' (notused 4) $ loggedc $ do
      x <- local $ return (notused 0)
      r <- onAll $ runCloud (serv x) <** modify (\s -> s{execMode= Remote}) --setData Remote
      local $ return r
      teleport
      return r

#endif

-- | call a service. If the service is not running in some node, the monitor service would install
-- and run it. The first parameter is a weak password.

#ifndef ghcjs_HOST_OS
callService
    :: (Subst1 a String, Loggable a,Loggable b)
    => Service -> a  -> Cloud b
callService service params = loggedc $ do 

    node <-  initService  service        !> ("callservice initservice", service)
    let type1 = fromMaybe "" $ lookup "type" service
    if type1=="HTTP" 
      then do
          callstr <- local $ emptyIfNothing $ lookup "HTTPstr" service
          callRestService node callstr params
      else callService'  node params            !> ("NODE FOR SERVICE",node)
#else
callService
    :: (Loggable a, Loggable b)
    =>  Service -> a  -> Cloud b
callService service params = local $ empty
#endif

setRemoteJob :: BSS.ByteString -> Node -> TransIO () 
setRemoteJob thid node= do
      JobGroup map  <- getRState <|> return (JobGroup M.empty)
      setRState $ JobGroup $ M.insert thid (node,0) map

data KillRemoteJob = KillRemoteJob BSS.ByteString deriving (Read,Show, Typeable)  
instance Loggable KillRemoteJob

killRemoteJob :: Node -> BSS.ByteString -> Cloud ()  
killRemoteJob node thid= callService' node (KillRemoteJob thid)


killRemoteJobIt :: KillRemoteJob -> Cloud ()
killRemoteJobIt (KillRemoteJob thid)= local $ do
      st <- findState match =<<  topState
      liftIO $ killBranch' st
      where
      match st= do
         (_,lab) <-liftIO $ readIORef $ labelth st
         return $ if lab == thid then True else False


-- | notify the the monitor that a node has failed for a service and reclaim another
-- to execute the request. If the service is not running in some node, the monitor service would install
-- and run it. The first parameter is a weak password.
callServiceFail
    :: (Typeable a , Typeable b, Loggable a, Loggable b)
    =>  Node -> a  -> Cloud b
#ifndef ghcjs_HOST_OS
callServiceFail  node params = loggedc $ do
    node <- reInitService  node     
    callService'  node params
#else
callServiceFail  node params = local empty
#endif

monitorNode= unsafePerformIO $ createNodeServ "localhost"
            (fromIntegral monitorPort)
            monitorService


-- | call a service located in a node
callService' :: (Loggable a, Loggable b) =>  Node -> a -> Cloud b 
#ifndef ghcjs_HOST_OS
callService' node params =  loggedc $ do
    onAll abduce
    
    my <- onAll getMyNode    -- to force connection when calling himself
    if node== my 
      then  onAll $ do
          svs <- liftIO $ readIORef selfServices
          
          modifyData' (\log -> log{buildLog=mempty,recover=True}) $ error "No log????"
          withParseString (toLazyByteString $ serialize params <> byteString (BSS.pack "/")) $ runCloud' svs
          modifyData' (\log -> log{recover=True}) $ error "No log????"
          log <- getState
          setParseString $ toLazyByteString $ buildLog log
          r <- logged empty
          
          return r   

      else do
          localFixServ True False
          local $ return ()       

          r <- wormhole' node $  do
    
             local $ return  params
    
             teleport
    
             r <- local empty -- read the response
             onAll $ symbol $ BS.pack "e/" 
             return r
          delData  (undefined :: LocalFixData)
          return r



       
             

   
          
                    
    -- on exception, callService is called to reclaim a new node to the monitor if necessary
    
 ---- `catchc` \(e :: SomeException ) -> do onAll $ delNodes [node] ; callServiceFail   node params
    
 {-   
 typea :: a -> Cloud a
 typea = undefined
 restoreLog (Log _ _ logw hash)= onAll $ do
       Log _ _ logw' hash' <- getSData <|> return emptyLog

       let newlog= reverse logw' ++ logw
--       return ()                 !> ("newlog", logw,logw')
       setData $ Log False newlog newlog (hash + hash')
-}
#else
callService'  node params = local empty
#endif

sendStatusToMonitor :: String -> Cloud ()
#ifndef ghcjs_HOST_OS
sendStatusToMonitor  status= loggedc $ do
       local $ onException $ \(e:: ConnectionError) -> continue >>  startMonitor    -- !> ("EXCEPTIOOOOOOOOOOON",e)
       nod <- local getMyNode
       callService'   monitorNode (nodePort nod, status) -- <|> return()
#else
sendStatusToMonitor  status= local $ return ()

inputAuthorizations :: Cloud ()
inputAuthorizations= empty
#endif


-- emptyLog= Log False [] [] 0

catchc :: Exception e => Cloud a -> (e -> Cloud a) -> Cloud a
catchc a b= Cloud $ catcht (runCloud' a) (\e -> runCloud' $ b e)


selfServices= unsafePerformIO $ newIORef empty
notused n= error $  "runService: "++ show (n::Int) ++ " variable should not be used"

-- | executes a program that export endpoints that can be called with `callService` primitives.
-- It receives the service description, a default port, the services to set up and the computation to start.
-- for example the monitor exposes two services, and is started with:
--
-- >  main = keep $ runService monitorService 3000 $
-- >                       [serve returnInstances
-- >                       ,serve addToLog] pings
--
-- every service incorporates a ping service and a error service, invoqued when the parameter received
-- do not match with any of the endpoints implemented.
runService :: Loggable a => Service -> Int -> [Cloud ()] -> Cloud a  -> TransIO ()
runService servDesc defPort servs proc= runCloud $
   runService' servDesc defPort servAll  proc
   where 
   servAll :: Cloud ()
   servAll =  foldr  (<|>) empty $ servs 
                ++ [ serve killRemoteJobIt
                   , serve ping 
                   , serve  (local . addNodes)
                   , serve getNodesIt
#ifndef ghcjs_HOST_OS
                   , serve redirectOutputIt
                   , serve sendToInputIt
#endif
                   , serveerror]

   ping :: () -> Cloud ()
   ping = const $ return() !> "PING"

   serveerror = empty -- localIO $  do
      -- error $  "parameter mismatch calling service  (parameter,service): "++ show (d :: IDynamic,servDesc)
      --  empty


data GetNodes = GetNodes deriving(Read,Show, Typeable)
instance Loggable GetNodes

-- | return the list of nodes known by the service
getNodesIt :: GetNodes -> Cloud [Node]
getNodesIt _ = local  getNodes



runService' :: Loggable a => Service -> Int -> Cloud () -> Cloud  a -> Cloud ()
runService' servDesc defPort servAll proc=  do

       onAll $ liftIO $ writeIORef selfServices servAll
       serverNode <- initNodeServ servDesc 
       wormhole' serverNode $ inputNodes <|> proc >> empty >> return()
       return () !> "ENTER SERVALL"
       onAll $ symbol $ BS.pack "e/"
       servAll'   
       teleport  

       where

       servAll' = servAll 

              `catchc` \(e:: SomeException ) -> do
                   setState emptyLog
                   return () !> ("ERRORRRRRR:",e)
                   node <- local getMyNode
                   sendStatusToMonitor  $ show e

                   local $ do
                      Closure closRemote <- getData `onNothing` error "teleport: no closRemote"
                      conn <- getData `onNothing` error "reportBack: No connection defined: use wormhole"
                      noTrans $ msend conn  $ SError $ toException $ ErrorCall $ show $ show $ CloudException node closRemote $ show e
                      empty -- return $ toIDyn ()
 



       initNodeServ servs=do
          (mynode,serverNode) <- onAll $ do
            node <- getNode "localhost" defPort servDesc 
            addNodes  [node]
            serverNode <- getWebServerNode 
            mynode <- if isBrowserInstance
                      then  do
                        addNodes [serverNode]
                        return node
                      else return serverNode

            conn <- defConnection
            liftIO $ writeIORef (myNode conn) mynode
             
            setState conn
            return (mynode,serverNode)
             
          inputAuthorizations <|> return ()

          listen mynode <|> return ()
          return serverNode
    
          where
          
          -- getNode :: TransIO Node
          getNode host port servs= def <|> getNodeParams <|> getCookie
              where
              def= do
                    args <- liftIO  getArgs

                    if "-p" `elem` args then empty else liftIO $ createNodeServ host port servs
              getNodeParams=
                if isBrowserInstance then liftIO createWebNode else do
                  oneThread $ option "start" "re/start node"
                  host <- input' (Just "localhost") (const True) "hostname of this node (must be reachable) (\"localhost\"): "
                  port <- input' (Just 3000) (const True)  "port to listen? (3000) "
                  liftIO $ createNodeServ host port servs
                  
#ifndef ghcjs_HOST_OS
          getCookie= do
            if isBrowserInstance then return() else do
              option "cookie" "set the cookie"
              c <- input (const True) "cookie: "
              liftIO $ writeIORef rcookie  c
            empty
#else
          getCookie= empty
#endif

-- | ping a service in a node. since services now try in other nodes created by the monitor until  succees, ping can be
-- used to preemptively assure that there is a node ready for the service.
ping node= callService' node ()  :: Cloud ()

sendToNodeStandardInput :: Node -> String -> Cloud ()
sendToNodeStandardInput node cmd= callService' (monitorOfNode node) (node,cmd) :: Cloud ()
   
-- | monitor for a node is the monitor process that is running in his host
monitorOfNode node= 
  case lookup "relay" $ nodeServices node of
      Nothing -> node{nodePort= 3000, nodeServices=monitorService}
      Just info ->  let (h,p)= read info 
                    in Node h p Nothing monitorService
  
data ReceiveFromNodeStandardOutput= ReceiveFromNodeStandardOutput Node BSS.ByteString deriving (Read,Show,Typeable)
instance Loggable ReceiveFromNodeStandardOutput

receiveFromNodeStandardOutput :: Node -> BSS.ByteString -> Cloud String
receiveFromNodeStandardOutput node ident= callService' (monitorOfNode node) $ ReceiveFromNodeStandardOutput node ident



-- | execute the individual services. A service within a program is invoked if the types of
-- the parameters received match with what the service expect. See `runService` for a usage example

serve :: (Loggable a, Loggable b) => (a -> Cloud b) -> Cloud ()
serve  serv= do 
        return () !> "SERVE"

        modify $ \s -> s{execMode= Serial}
        p <-  onAll deserialize --  empty if the parameter does not match
        modifyData' (\log -> log{recover=False}) $ error "serve: error"
        loggedc $ serv p
        return()


#ifndef ghcjs_HOST_OS 


-- callRestService :: (Subst1 a String, fromJSON b) =>  Node -> String -> a -> Cloud ( BS.ByteString)
callRestService node callString vars=  local $ do
  newVar "hostnode"  $ nodeHost node
  newVar "hostport"  $ nodePort node
  let calls = subst callString vars
  restmsg <- replaceVars calls
  return () !> ("restmsg",restmsg)
  rawREST node  restmsg




controlNodeService node=  send <|> receive
      where
      send= do
         local abduce 
         local $ do
            let nname= nodeHost node ++":" ++ show(nodePort node)
            
            liftIO $ putStr "Controlling node " >> print nname
            liftIO $ writeIORef  lineprocessmode True
            oldprompt <- liftIO $ atomicModifyIORef rprompt $ \oldp -> ( nname++ "> ",oldp)
            cbs <- liftIO $ atomicModifyIORef rcb $ \cbs -> ([],cbs) -- remove local node options
            setState (oldprompt,cbs)                                             -- store them
            
            
         endcontrol <|> log <|> inputs
         empty
         
      endcontrol= do
        
         local $ option "endcontrol"  "end controlling node"
         killRemoteJob (monitorOfNode node) $ controlToken
         local $ do
            liftIO $ writeIORef lineprocessmode False
            liftIO $ putStrLn "end controlling remote node"
            (oldprompt,cbs) <- getState
            liftIO $ writeIORef rcb cbs -- restore local node options
            liftIO $ writeIORef rprompt  oldprompt
              
      log = do
              local $ option "log" "display the log of the node"
              log <- Transient.Move.Services.getLog node
              localIO $ do
                 
                 putStr "\n\n------------- LOG OF NODE: ">> print node >> putStrLn ""
                 mapM_ BS.putStrLn $ BS.lines log
                 putStrLn   "------------- END OF LOG"

      inputs= do           
          line <- local $ inputf False "input"  Nothing (const True)  
          sendToNodeStandardInput node line


      receive=  do
         local $ setRemoteJob controlToken $ monitorOfNode node
         r <- receiveFromNodeStandardOutput node $ controlToken
         when (not $ null r) $ localIO $ putStrLn  r 
         empty


controlNode node= send <|> receive
      where
      send= do
         local abduce
         local $ do
            let nname= nodeHost node ++":" ++ show(nodePort node)
            liftIO $ writeIORef lineprocessmode True
            liftIO $ putStr "Controlling node " >> print nname
            
            oldprompt <- liftIO $ atomicModifyIORef rprompt $ \oldp -> ( nname++ "> ",oldp)
            cbs <- liftIO $ atomicModifyIORef rcb $ \cbs -> ([],cbs) -- remove local node options
            setState (oldprompt,cbs)                                             -- store them
            
            
         endcontrol <|> log <|> inputs
         empty
         
      endcontrol= do
         local $ option "endcontrol"  "end controlling node"
         killRemoteJob  node $ controlToken
         local $ do
            liftIO $ writeIORef lineprocessmode False
            liftIO $ putStrLn "end controlling remote node"
            (oldprompt,cbs) <- getState
            liftIO $ writeIORef rcb cbs -- restore local node options
            liftIO $ writeIORef rprompt  oldprompt
              
      log = do
              local $ option "log" "display the log of the node"
              log <- Transient.Move.Services.getLog node
              localIO $ do
                 
                 putStr "\n\n------------- LOG OF NODE: ">> print node >> putStrLn ""
                 mapM_ BS.putStrLn $ BS.lines log
                 putStrLn   "------------- END OF LOG"

      inputs= do           
          line <- local $ inputf False "input"  Nothing (const True)  
          callService' node $ SendToInput line :: Cloud ()


      receive=  do
         local $ setRemoteJob controlToken $ monitorOfNode node
         r <- callService' node $ RedirectOutput $ controlToken
         localIO $ putStrLn  r
         empty

{-# NOINLINE controlToken#-}
controlToken :: BSS.ByteString
controlToken= fromString "#control" <> fromString (show (unsafePerformIO $ (randomIO :: IO Int)))

newtype RedirectOutput= RedirectOutput BSS.ByteString deriving (Read,Show,Typeable)
instance Loggable RedirectOutput

newtype SendToInput= SendToInput String deriving (Read,Show,Typeable)
instance Loggable SendToInput

sendToInputIt :: SendToInput -> Cloud ()
sendToInputIt (SendToInput input)= localIO $  processLine input >> hFlush stdout -- to force flush stdout

redirectOutputIt  (RedirectOutput label)= local $ do
   
   (rr,ww) <- liftIO createPipe 
   stdout_dup <- liftIO $ hDuplicate stdout
   liftIO $ hDuplicateTo  ww stdout  
   finish stdout_dup  
   labelState label
   read rr 
   where
   read rr = waitEvents $  hGetLine rr 
   
   finish stdout_dup = onException $ \(e :: SomeException) -> do

     liftIO $ hDuplicateTo stdout_dup stdout
     liftIO $ putStrLn "restored control"
     empty



         
newtype GetLog= GetLog Node deriving (Read,Show, Typeable)
instance Loggable GetLog

getLog :: Node  -> Cloud BS.ByteString
getLog node= callService' (monitorOfNode node) (GetLog node)


-------------------cloudshell vars -------------------------
data LocalVars = LocalVars (M.Map String String) deriving (Typeable, Read, Show)


newVar :: (Show a, Typeable a) => String -> a -> TransIO () 
newVar  name val= noTrans $ do 
   LocalVars map <- getData `onNothing` return (LocalVars M.empty)
   setState $ LocalVars $ M.insert  name (show1 val) map

replaceVars :: String -> TransIO String
replaceVars []= return []
replaceVars ('$':str)= do
   LocalVars localvars <- getState <|> return (LocalVars M.empty)
   let (var,rest')= break (\c -> c=='-' || c==' ' ||  c=='\r' || c == '\n' ) str
       (manifest, rest)= if null rest' || head rest'=='-' 
            then  break (\c -> c=='\r' || c =='\n' || c==' ') $ tailSafe rest'
            else  ("", rest')

   if var== "port"&& null manifest then (++) <$> (show <$> freePort) <*> replaceVars rest   -- $host variable
   else if var== "host" && null manifest then (++) <$> (nodeHost <$> getMyNode) <*> replaceVars rest
   else if null manifest  then
      case M.lookup var localvars of
          Just v -> do 
              v' <- processVar v
              (++) <$> return (show1 v') <*> replaceVars rest
          Nothing -> (:) <$> return '$' <*> replaceVars rest 
   else do
      map <- liftIO $ readFile manifest >>= return . toMap
      let mval = lookup var map 
      case mval of 
        Nothing -> error $ "Not found variable: "++ "$" ++ var ++ manifest 
        Just val -> (++) <$> return val <*> replaceVars rest
   where
   tailSafe []=[]
   tailSafe xs= tail xs
   
   processVar= return . id
   
   toMap :: String -> [(String, String)]
   toMap desc= map break1 $ lines desc
     where
     break1 line=
        let (k,v1)= break (== ' ') line
        in (k,dropWhile (== ' ') v1)

replaceVars (x:xs) = (:) <$> return x <*> replaceVars xs 

---------------- substitution ---------------------------------------------

subst :: Subst1 a r => String -> a -> r
subst expr= subst1 expr 1


class Subst1 a r where 
    subst1 :: String -> Int -> a -> r

   
instance (Show b, Typeable b, Subst1 a r) => Subst1 b (a -> r) where
    subst1 str n x = \a -> subst1 (subst1 str n x) (n+1) a

instance {-# Overlaps #-} (Show a,Typeable a, Show b, Typeable b) => Subst1 (a,b) String where
    subst1 str n (x,y)= subst str x y
    
instance {-# Overlaps #-} (Show a,Typeable a, Show b, Typeable b
                          ,Show c, Typeable c) => Subst1 (a,b,c) String where
    subst1 str n (x,y,z)= subst str x y z
    
instance {-# Overlaps #-} (Show a,Typeable a, Show b, Typeable b
                          ,Show c,Typeable c, Show d, Typeable d) 
                           => Subst1 (a,b,c,d) String where
    subst1 str n (x,y,z,t)= subst str x y z t
    

instance {-# Overlaps #-}  (Show a,Typeable a) => Subst1 a String where
     subst1 str n x= subst2 str n x 
    
subst2 str n x=  replaces str ('$' : show n ) x

replaces str var x= replace var (show1 x) str  

replace _ _ [] = []
replace a b s@(x:xs) = 
                   if isPrefixOf a s
                            then b++replace a b (drop (length a) s)
                            else x:replace a b xs




show1 :: (Show a, Typeable a) => a -> String     
show1 x | typeOf x == typeOf (""::String)= unsafeCoerce x 
          | otherwise= show x 
#endif
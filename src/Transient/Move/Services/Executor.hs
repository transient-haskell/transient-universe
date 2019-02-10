module Transient.Move.Services.Executor where
 
import Transient.Internals
import Transient.Move.Internals
import Transient.Move.Services
import Data.IORef
import System.IO.Unsafe
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8   as BS
import qualified Data.ByteString.Char8 as BSS
import Data.String
import Data.Typeable
import Control.Applicative
import Control.Monad
import Control.Monad.State (liftIO)
    
executorService = [("service","executor")
                  ,("executable", "executor")
                  ,("package","https://github.com/transient-haskell/transient-universe")]



-- initialize N instances, of the executor service. The monitor would spread them among the nodes available.
-- the number N should be less of equal than the number of phisical machines.
-- Since the executor serivice can execute any number of processes, it sould be at most one per machine. 

initExecute  number=  requestInstance   executorService  number

-- | execute a command in some node by an executor service, and return the result when the program finishes
networkExecute ::  String -> String ->  Cloud String
networkExecute  cmdline input= 
     callService  executorService (cmdline, input,())
     


-- | execute a process in some machine trough the local monitor and the executor.
-- This call return a process identifier
-- The process can be controlled with other services like `controlNodeProcess`
networkExecuteStream'  :: String -> Cloud String
networkExecuteStream'  cmdline= do
    -- callService  executorService cmdline
     node <- initService  executorService
     return () !> ("STORED NODE", node) 
     name <- callService' node $ ExecuteStream cmdline
     localIO $ print ("NAME", name)
     localIO $ atomicModifyIORef rnodecmd $ \map -> (M.insert name node map,())
     local $ setRemoteJob (BSS.pack name) node         -- so it can be stopped by `killRemoteJob`
     return name 
     
-- | execute a shell command in some node using the executor service. 
-- The response is received as an stream of responses, one per line
networkExecuteStream :: String -> Cloud String      -- '[Multithreaded,Streaming]
networkExecuteStream  cmdline= do
         node <- initService  executorService
         flag <- onAll $ liftIO $ newIORef False
         r <- callService'  node cmdline
         init <- onAll $ liftIO $ readIORef flag
         when (not init) $ do
            onAll $ liftIO $ writeIORef flag True   -- get the first line (header) as the name of the process
            local $ setRemoteJob  (BSS.pack r) node         -- so it can be stopped by `killRemoteJob`
            localIO $ atomicModifyIORef rnodecmd $ \map -> (M.insert r node map,())
         return r 
         
rnodecmd= unsafePerformIO $ newIORef M.empty

-- | send a message that will be read by the standard input of the program initiated by `networkExecuteStream`, identified by the command line.
-- the stream of responses is returned by that primitive. `sendExecuteStream` never return anything, since it is asynchronous
sendExecuteStream :: String -> String -> Cloud  ()  -- '[Asynchronous]
sendExecuteStream  cmdline msg=  do

     return () !> ("SENDEXECUTE", cmdline)
     node <- nodeForProcess cmdline
      --localIO $ do
      -- map <- readIORef rnodecmd 
      -- let mn = M.lookup cmdline map 
      -- case mn of
      --   Nothing ->  error $ "sendExecuteStream: no node executing the command: "++ cmdline
      --   Just n -> return n
     return () !> ("NODE", node)
     callService'  node (cmdline, msg)
     
     
controlNodeProcess cmdline= do
      exnode <- nodeForProcess cmdline
        --local $ do
        --    map <-  readIORef rinput 
        --    let mn = M.lookup cmdline map 
        --    return $ case mn of
        --           Nothing ->  error $ "sendExecuteStream: no node executing the command: "++ cmdline
        --           Just n ->   n
                   
      send exnode  <|>  receive exnode
      where

      send exnode= do
         local abduce
         local $ do
            liftIO $ writeIORef lineprocessmode True
            oldprompt <- liftIO $ atomicModifyIORef rprompt $ \oldp -> ( takeWhile (/= ' ') cmdline++ "> ",oldp)
            cbs <- liftIO $ atomicModifyIORef rcb $ \cbs -> ([],cbs) -- remove local node options
            setState (oldprompt,cbs)                                             -- store them

            
         endcontrolop exnode <|> kill exnode <|> log exnode <|> inputs exnode 
         empty
         
      kill exnode= do
         local $ option "kill" "kill the process"
         localIO $ putStrLn "process terminated"
         killRemoteJob exnode $ fromString cmdline
         endcontrol exnode
         
      endcontrolop exnode= do
         local $ option "endcontrol"  "end controlling the process"
         localIO $ putStrLn "end controlling the process"
         endcontrol exnode
         
      endcontrol exnode= do
         localIO $ writeIORef lineprocessmode False
         killRemoteJob exnode  controlToken
         local $ do

            (oldprompt,cbs) <- getState
            liftIO $ writeIORef rcb cbs -- restore local node options
            liftIO $ writeIORef rprompt  oldprompt
              
      log exnode = do
              local $ option "log" "display the log of the node"
              log <- getLogCmd cmdline exnode
              localIO $ do
                 
                 putStr "\n\n------------- LOG OF PROCESS: ">> print cmdline >> putStrLn ""
                 mapM_ BS.putStrLn $ BS.lines log
                 putStrLn   "------------- END OF LOG"
                 
      inputs exnode= do  

          line <- local $ inputf False "input"  Nothing (const True)  
          sendExecuteStream cmdline line
         
      
      receive exnode=  do
         r <- receiveExecuteStream cmdline exnode
         when (not $ null r) $ localIO $ putStrLn  r  
         empty

      receiveExecuteStream cmd node=do
          local $ setRemoteJob  controlToken node
          callService'  node $ ReceiveExecuteStream cmd controlToken 

      getLogCmd :: String -> Node -> Cloud BS.ByteString
      getLogCmd cmd node= callService' node (GetLogCmd cmd)

newtype GetLogCmd= GetLogCmd String  deriving (Read, Show, Typeable)
newtype ExecuteStream= ExecuteStream String deriving (Read, Show, Typeable)
data    ReceiveExecuteStream= ReceiveExecuteStream String BSS.ByteString deriving (Read, Show, Typeable)
data    GetProcesses= GetProcesses deriving (Read, Show, Typeable)

getProcesses ::  Node -> Cloud [String]
getProcesses node= callService' node GetProcesses





-- | get the executor that executes a process


nodeForProcess :: String -> Cloud Node
nodeForProcess process= do 

  callService monitorService () :: Cloud ()  -- start/ping monitor if not started
  
  nods <- squeezeMonitor [] monitorNode
  case nods of
    []    -> error $ "no node running: "++ process
    nod:_ -> return nod
  where
  squeezeMonitor :: [Node] -> Node -> Cloud [Node]
  squeezeMonitor  exc nod= do
    if  nod `elem` exc then return [] else do
      nodes <- callService' nod GetNodes :: Cloud [Node]
      return . concat =<< mapM squeeze (tail nodes) 
      
      where
      squeeze :: Node -> Cloud [Node]
      squeeze  node= do

          case lookup "service" $ nodeServices node of
           
                Just "monitor" -> squeezeMonitor (nod:exc)  node

                Just "executor" -> do

                    procs <- callService' node GetProcesses :: Cloud [String]
                    if process `elem` procs then return [node] else return []

                _ -> return []
      
      

 
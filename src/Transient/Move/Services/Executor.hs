module Transient.Move.Services.Executor where

import Transient.Internals
import Transient.Move.Internals
import Transient.Move.Services
import Data.IORef
import System.IO.Unsafe
import qualified Data.Map as M
    
executorService = [("service","executor")
                  ,("executable", "executor")
                  ,("package","https://github.com/transient-haskell/transient-universe")]



-- initialize N instances, of the executor service. The monitor would spread them among the nodes available.
-- the number N should be less of equal than the number of phisical machines.
-- Since the executor serivice can execute any number of processes, it sould be at most one per machine. 

initExecute ident number=  requestInstance  ident executorService  number

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
     
     





{-# LANGUAGE   ScopedTypeVariables #-}


import Transient.Internals
import Transient.Move.Internals
import Transient.Indeterminism
import Transient.Move.Utils
import Transient.Logged
import Transient.Move.Services
import Transient.Move.Services.Executor
import Control.Applicative
import Control.Monad

import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Control.Exception hiding (onException)
import System.IO.Unsafe
import Data.Maybe


import System.IO
import System.Process
import Control.Concurrent
{-  


 example record updates, distributed database?
 connect. Un servicio para conectar añadir instancias?
 
 
 connect created instances 
       connectNode as service.

     
a transient service as REST service: in the http treatment in listen: /a/b/c/d -> (a,b,c,d)

 option to discover the types of service parameters:
     get the services
     return the types
 
 -}
 






     


       



main = keep $ runService [("executable","testService")] 8000 [serve selfService] $ do
    control2 <|> examples 
    
    
examples= do
    local $ option "examples"  "some examples and test of service usage"
    ping1 <|> ping2 <|> singleExec <|> stream <|> 
     failThreeTimes <|> many1 <|> fail3requestNew <|> 
     requestAtHost <|> self  <|> distrib

distrib= do
  local $ option "dis" "request another instance of this program and call it"
  this <- local getMyNode
  localIO $ print this
  [node] <- requestInstance (nodeServices this) 1
  local $ option "launch" "launch"
  r <- runAt node $ return "hello world"
  localIO $ print r

  
control2 = control <|> spawn1

spawn1= do
    local $ option "spawn" "spawn a bash shell and a loop that can be visited/controlled"
    networkExecuteStream' "bash"
    networkExecuteStream' "./loop.sh  'hello'"
    localIO $ putStrLn "SPAWNED\n\nUse \"control\" to manage the processes"
    empty

control=  do
  local $ option "control" "control a node or process initiated by previous examples"
  cloudControl
  
  
cloudControl= do
  localIO $ putStrLn "\n...........VISIBLE NODES AND PROCESSES............"
 
  callService monitorService () :: Cloud ()  -- start/ping monitor if not started
  
  localIO $ do

              putStr $ nodeHost monitorNode
              putChar ':'
              putStr $ show $ nodePort monitorNode
              putChar '/'
              putStrLn $ fromJust $ lookup "service" $ nodeServices monitorNode
  squeezeMonitor 4 monitorNode
  where

  squeezeMonitor  tab nod= do
      nodes <- callService' nod GetNodes :: Cloud [Node]

              
      vis <- local $ do
          visited <- getState <|> return []
          let vis = nod `elem` visited
          when (not vis) $ setState $ nod:visited
          return vis
      when (not vis) $ spawn $ controlMonitor nod
                           
      mapM_ squeeze $ tail nodes
      where

      squeeze  node= do
       vis <- local $ do
          visited <- getState <|> return []
          let vis= node `elem` visited
          when (not vis) $ setState $ node:visited
          return vis
       when (not vis) $ do
           localIO $ do
              putStr $ take tab $ repeat ' '
              putStr $ nodeHost node
              putChar ':'
              putStr $ show $ nodePort node
              putChar '/'
              putStrLn $ fromJust $ lookup "service" $ nodeServices node


              
           case lookup "service" $ nodeServices node of
           
                Just "monitor" -> do 
                        spawn $ controlMonitor node
                        visited <- local $ getState <|> return []
                        when (not $ node `elem` visited) $ do
                           local $ setState $ node:visited
                           
                           localIO $ do
                              putStr $ take tab $ repeat ' '
                              putStr "   "
                              putStrLn "Services:" 
                           squeezeMonitor  (tab+4)  node

                Just "executor" -> do
                    spawn $ controlService node
                    procs <- callService' node GetProcesses :: Cloud [String]
                    

                    when (not $ null procs) $ do
                      localIO $ do
                        putStr $ take tab $ repeat ' '
                        putStrLn "  Running processes:"
                      mapM_ ( spawn . controlProcess) procs
                      
                _ -> return ()
      
      controlMonitor node=  do
            local $ do
               n <- getState <|> return (0 :: Int) 
               setState $ n +1
               liftIO $ putStr "\t\t"
               option1 n  "control this node\n" 
               abduce      
            controlNode node
               
      controlService node= do
           local $ do
               n <- getState <|> return (0 :: Int) 
               setState $ n +1
               liftIO $ putStr "\t\t"
               option1 n "control this node\n"
               abduce

           controlNodeService node 
           
      spawn f=  (f >> empty) <|> return ()
      
      controlProcess str= do              
              local $ do
                 n <- getState <|> return (0 :: Int)
                 setState $ n +1
                 liftIO $ do
                     putStr $ take tab $ repeat ' '
                     putStr "   "
                     putStrLn str
                     putStr "\t\t"
                 option1 n "control this process\n"
                 abduce
              controlNodeProcess str

 


{-
registerUpdate= do
   local $ option "reg" "simulate a two way reactive database update service"
   reg <- input (const True) "enter register content "
   reg' <- updateDistributedDatabase reg    
   localIO $ putStr "new register changed: " >> putStrLn reg'

in the service, made by the same service executable running in different machines and connected among them:

   updateDistributedDatabaseIt= clustered $ do
               update reg
               return reg
-} 

self= do
  local $ option "own"  "call a service of my own program"
  
  nod <- local $ getMyNode
  
  r <- callService' nod "Alberto" :: Cloud String
  localIO  $ print r
  
selfService str = localIO $ return $ "hello " ++ str

ping1 = do
        local $ option "ping1" "ping monitor (must have been started)"
        r <- callService' monitorNode ()
        localIO $ print (r :: ())
        
        
ping2 = do
        local $ option "ping" "ping two executors, must return: [((),())]"

        ns <- requestInstance executorService 2
        r <- mapM ping ns
        localIO $ print r
         

          
singleExec= do
        local $ option "single" "execution of \"ls -al\" in a executor process"
        r <- networkExecute "ls -al" ""
        localIO $ print ("RESULT",r)




stream= do
          local $ setRState False
          local $ option "stream"  "start a remote shell with the executor, then executes different command inputs and stream results"
          r <- networkExecuteStream "bash"
          s <- local getRState 
          if s then localIO $ putStr "[bash]" >> print r
               else  do
                 local $ setRState True
                 inputs r  -- the first output of the command is the process identifier
  where      
  inputs idproc= do
        command <- local $ do
           option "send" "send to the remote shell"
           input (const True) "command"
        sendExecuteStream idproc command
        empty
        
        
fail3requestNew=  do
    local $ option "fail6"  "try a new instance"

    retries <- onAll $ liftIO $ newIORef (0 :: Int)
    
    local $ onException $ retry6 retries

    r <- networkExecute "UNKNOWN COMMAND" ""

    localIO $ print ("LINE=",r :: String )

    where
    retry6 retries (CloudException node _ _ )= runCloud $ do
         localIO $ print ("tried to execute in", node)
         n <- onAll $ liftIO $ atomicModifyIORef retries $ \n -> (n+1,n+1)
         localIO $ print ("NUMBER OF RETRIES",n)
         
         if n == 3 then do
               localIO $ putStrLn "failed after three retries, reclaiming new instance"
               local continue
               [node'] <- requestInstanceFail node  1
               localIO $ print ("NEW NODE FOR SERVICE", node')

         else if  n < 6  then local continue 
         
         else  localIO $ print "failed after six retries with two instances, aborting"


failThreeTimes=  do
    local $ option "fail"  "fail after three retries"
    
 
    retries <- onAll $ liftIO $ newIORef (0 :: Int)
    
    let retry3 e=  do
         liftIO $ print e
         n <- liftIO $ atomicModifyIORef retries $ \n -> (n+1,n+1)
         liftIO $ print ("NUMBER OF RETRIES",n)
         if n < 3 then continue else  do
               liftIO $ print "failed after three retries"
               empty

    local $ onException $ \(e :: CloudException) -> retry3 e
    
    r <- networkExecute "UNKNOWN COMMAND" ""

    localIO $ print ("LINE=",r :: String )
 
many1=  do
        local $ option "many" "show how a command is tried to be executed in different executor instances"
        requestInstance executorService 5
        retries <- onAll $ liftIO $ newIORef (0 :: Int)
                   
        local $ onException $ \e  -> retry1 5 retries e   
        
        networkExecute "unknow command" ""
 
        return ()
        
        where
        retry1 n' retries (CloudException node _ _ )=  do
             liftIO $ print ("tried to execute in", node)
             n <- liftIO $ atomicModifyIORef retries $ \n -> (n+1,n+1)
             liftIO $ print ("NUMBER OF RETRIES",n)
             if n < n' then continue else  do
                   liftIO $ putStr  "stop after " >> putStr (show n) >> putStrLn "retries"
                   empty 

requestAtHost= do
       local $ option "host"  "request the execution of a shell process at a given machine"
       hostname <- local $ input (const  True)  "enter the hostname (the machine should have monitorService running at port 3000) "
       process <- local $ input (const  True)  "enter the process to run (for example: bash) "
       line <- atHost hostname process  <|> inputCommands process
       localIO $ print ("LINE", line) 
       where
       inputCommands process= do

              local $ option "inp" "enter input for the process created"
              inp <- local $ input  (const True) "input string: " :: Cloud String
              callService executorService (process, inp) :: Cloud() 
              empty
   
       atHost :: String -> String -> Cloud String
       atHost hostname process = do
               executor <- requestInstanceHost hostname executorService
               callService' executor process
   
   
   
   
   

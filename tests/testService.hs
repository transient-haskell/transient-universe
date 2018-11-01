{-# LANGUAGE   CPP, ScopedTypeVariables #-}


import Transient.Internals
import Transient.Move.Internals
import Transient.Move.Utils
import Transient.Logged
import Transient.Move.Services
import Transient.Move.Services.Executor
import Control.Applicative
import Control.Monad

import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.State
import Control.Exception hiding (onException)



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


main= keep $ runService [] 8000 [serve selfService] $ do   
    ping1 <|> ping2  <|> singleExec <|> stream <|> 
     failThreeTimes <|> many1 <|> fail3requestNew <|> 
      requestAtHost <|> self
      
selfService str = localIO $ return $ "hello " ++ str

{-
register= do
   local $ option "reg" "simulate a two way reactive database update service"
   reg <- input (const True) "enter register content "
   reg' <- update reg
   localIO $ putStr "new register changed: " >> putStrLn reg'
-} 

self= do
  local $ option "own"  "call a service of my own program"
  
  nod <- local $ getMyNode

  
  r <- callService' "" nod "Alberto" :: Cloud String
  localIO  $ print r
  
ping1 = do
        local $ option "ping1" "ping monitor (must have been started"
        r <- callService' "" monitorNode ()
        localIO $ print (r :: ())
        
        
ping2 = do
        local $ option "ping" "ping two executors, must return: [((),())]"
        ns <- requestInstance "" executorService 2
        r <- mapM ping ns
        localIO $ print r
          
singleExec= do
        local $ option "single" "execution of \"ls -al\" in a executor process"
        r <- networkExecute "" "ls -al" ""
        localIO $ print ("RESULT",r)




stream= do
          local $ option "stream"  "start a remote shell with the executor, then executes different command inputs and stream results"
          r <- shellExec <|> inputs
          localIO $ print ("LINE",r)
  where
  shellExec=  networkExecuteStream "" "bash"

          
  inputs= do
        command <- local $ do
           option "send" "send to the remote shell"
           input (const True) "command"
        sendExecuteStream "" "bash" command
        empty
        
        
fail3requestNew=  do
    local $ option "fail6"  "try a new instance"

    retries <- onAll $ liftIO $ newIORef (0 :: Int)
    
    local $ onException $ retry6 retries

    r <- networkExecute "" "UNKNOWN COMMAND" ""

    localIO $ print ("LINE=",r :: String )

    where
    retry6 retries (CloudException node _ _ )= runCloud $ do
         localIO $ print ("tried to execute in", node)
         n <- onAll $ liftIO $ atomicModifyIORef retries $ \n -> (n+1,n+1)
         localIO $ print ("NUMBER OF RETRIES",n)
         
         if n == 3 then do
               localIO $ putStrLn "failed after three retries, reclaiming new instance"
               local continue
               [node'] <- requestInstanceFail "" node  1
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
    
    r <- networkExecute "" "UNKNOWN COMMAND" ""

    localIO $ print ("LINE=",r :: String )
 
many1=  do
        local $ option "many" "show how a command is tried to be executed in different executor instances"
        requestInstance "" executorService 5
        retries <- onAll $ liftIO $ newIORef (0 :: Int)
                   
        local $ onException $ \e  -> retry1 5 retries e   
        
        networkExecute "" "unknow command" ""
 
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
              callService "" executorService (process, inp) :: Cloud() 
              empty
   
       atHost :: String -> String -> Cloud String
       atHost hostname process = do
               executor <- requestInstanceHost "" hostname executorService
               callService' "" executor process
   
   
   
   
   
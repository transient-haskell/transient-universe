-----------------------------------------------------------------------------
--
-- Module      :  Transient.Move.Services.Executor
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
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Transient.Internals
import Transient.Move.Services.Executor
import Transient.Move.Internals
import Transient.Move.Utils
import Transient.Logged(maybeFromIDyn)
import Transient.Move.Services
import Transient.Move.Services.Executor
import Control.Applicative
import Control.Monad.IO.Class
import Control.Exception(SomeException(..),catch)
import Control.Concurrent
import Control.Monad
import Data.List
import System.Process
import System.Directory
import Data.Monoid
import Data.IORef
import System.IO
import System.IO.Unsafe
import qualified Data.Map as M
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8   as BS
import qualified Data.ByteString.Char8 as BSS
import Data.String
import Data.Time


main = do
   putStrLn "Starting Transient Executor Service"
   keep $ runService executorService 3005  
                [ serve networkExecuteStreamIt
                , serve networkExecuteIt
                , serve sendExecuteStreamIt
                , serve receiveExecuteStreamIt
                , serve networkExecuteStreamIt'
                , serve getLogIt
                , serve getProcessesIt]
                empty 

getProcessesIt :: GetProcesses -> Cloud [String]
getProcessesIt _= localIO $ do
   map1 <- readIORef rinput
   return $ map fst $ M.toList map1
               
-- | send input to a remote process initiated with `networkExecuteStream` or `networkExecuteStream'`
sendExecuteStreamIt :: (String,String) -> Cloud ()
sendExecuteStreamIt (cmdline, inp)= do
   localIO $ do
       map <- readIORef rinput 
       let input= fromMaybe (error "this command line has not been opened") $ M.lookup cmdline map 
       hPutStrLn input inp 
       hFlush input 
   return ()

-- receive input from a remote process initiated with `networkExecuteStream'`
receiveExecuteStreamIt :: ReceiveExecuteStream -> Cloud String   
receiveExecuteStreamIt (ReceiveExecuteStream expr ident)= local $ do
     labelState ident 
     getMailbox' ("output"++ expr)

-- | execute a shell script and a input, and return all the output. Called externally by `networkExecute`
networkExecuteIt :: (String, String, ()) -> Cloud String
networkExecuteIt (expr, input,()) = localIO $ readCreateProcess (shell expr) input

getLogIt :: GetLogCmd -> Cloud BS.ByteString
getLogIt (GetLogCmd cmd)= localIO $ BS.readFile $ logFileName cmd


logFileName expr= subst expr ++ ".log"
    where
    subst []= [] 
    subst (' ':xs)= '-':subst xs
    subst ('/':xs)= '-':subst xs
    subst ('\"':xs)= '-':subst xs
    subst (x:xs)= x:subst xs

networkExecuteStreamIt' :: ExecuteStream  -> Cloud String
networkExecuteStreamIt' (ExecuteStream expr) = local $ do

   setRState False

   r <- executeStreamIt expr
    


   init <- getRState
   if init then empty
        else do
           setRState True
           return r       -- return the first output line only


-- execute the shell command specified in a string and stream line by line the standard output/error
-- to the service caller. It also store the output in a logfile and update a mailbox that can be
-- inspected by `receiveExecuteStreamIt`. Invoked by `networkExecuteStream`.
-- The first result returned is the process identifier.
networkExecuteStreamIt :: String  -> Cloud String
networkExecuteStreamIt expr  =  local $ executeStreamIt expr



executeStreamIt expr = do

      r <- liftIO $ createProcess $ (shell expr){std_in=CreatePipe,std_err=CreatePipe,std_out=CreatePipe}
      
      time <- liftIO $ getCurrentTime
      let header= expr ++"  "++ show time
      abduce
      labelState $ BSS.pack header

      
      onException $ \(e :: SomeException) ->  do 
             liftIO $ do
                 print ("watch:",e) 
                 cleanupProcess r 
                 atomicModifyIORef rinput $ \map -> (M.delete header  map,())
             empty 
      
      let logfile= logFileName  header 
      let box= "output" ++ header
      liftIO $ atomicModifyIORef rinput $ \map -> (M.insert header (input1 r) map,())
      
      line <- async (return header) <|> watch (output r) <|> watch (err r) <|> watchExitError r 
      
      putMailbox' box line   
      
      hlog <- liftIO $ openFile logfile AppendMode 
      liftIO $ hPutStrLn  hlog line
      liftIO $ hClose hlog    
      return line
      
      where

      input1 r= inp where (Just inp,_,_,_)= r
      output r= out where (_,Just out,_,_)= r
      err r= err where    (_,_,Just err,_)= r
      handle r= h where   (_,_,_,h)= r

      watch :: Handle -> TransIO String
      watch h = do
        abduce
        mline  <-  threads 0 $ (parallel $  (SMore <$> hGetLine' h) `catch` \(e :: SomeException) -> return SDone)
        case mline of
           SDone -> empty
           SError e -> do liftIO $ print ("watch:",e); empty
           SMore line ->  return line
           
        where

        hGetLine' h= do
          buff <- newIORef []
          getMore buff
          
          where

          getMore buff= do
            b <- hWaitForInput h 10
            if not b
                then do
                   r <-readIORef buff
                   if null r then getMore buff else return r
                else do
                      c <- hGetChar h
                      if c == '\n' then readIORef buff else do
                        modifyIORef buff $ \str -> str ++ [c]
                        getMore buff

      watchExitError r= do    -- make it similar to watch
        abduce
        liftIO $ waitForProcess $ handle r
        errors <- liftIO $  hGetContents (err r)
        return errors


rinput= unsafePerformIO $ newIORef M.empty 
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

import Transient.Move.Internals
import Transient.Move.Utils
import Transient.Logged(maybeFromIDyn)
import Transient.Move.Services
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

-- need as many instances as nodes
-- must execute each request in a random node.
-- solved: monitor return N instances one for node
-- networkExecute... select a different node each time.
-- sendExecute need to detect the executor instance.

main = do
   putStrLn "Starting Transient Executor Service"
   keep' . runCloud $ runService executorService 3005  
                [ serve networkExecuteStreamIt
                , serve networkExecuteIt
                , serve sendExecuteStreamIt]
                (local $ return()) 
                
sendExecuteStreamIt :: (String,String) -> Cloud ()
sendExecuteStreamIt (cmdline, inp)= do
   localIO $ do
       map <- readIORef rinput 
       let input= fromMaybe (error "this command line has not been opened") $ M.lookup cmdline map 
       hPutStrLn input inp 
       hFlush input
   return ()

networkExecuteIt :: (String, String, ()) -> Cloud String
networkExecuteIt (expr, input,()) = localIO $ readCreateProcess (shell expr) input



networkExecuteStreamIt :: String  -> Cloud String
networkExecuteStreamIt expr  = local $ do

      r <- liftIO $ createProcess $ (shell expr){std_in=CreatePipe,std_err=CreatePipe,std_out=CreatePipe}
      liftIO $ atomicModifyIORef rinput $ \map -> (M.insert expr (input1 r) map,())
      watch (output r) <|> watcherror r 
      
      where
      input1 r= inp where (Just inp,_,_,_)= r
      output r= out where (_,Just out,_,_)= r
      err r= err where    (_,_,Just err,_)= r
      handle r= h where   (_,_,_,h)= r

      watch :: Handle -> TransIO String
      watch h=    do
        mline  <-  threads 0 $ (parallel $  (SMore <$> hGetLine' h) `catch` \(e :: SomeException) -> return SDone)
        case mline of
           SDone -> empty
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
                      if c== '\n' then readIORef buff else do
                        modifyIORef buff $ \str -> str ++ [c]
                        getMore buff

      watcherror r= do    -- make it similar to watch
        abduce
        liftIO $ waitForProcess $ handle r
        errors <- liftIO $  hGetContents (err r)
        return errors


rinput= unsafePerformIO $ newIORef M.empty 
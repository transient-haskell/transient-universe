#!/usr/bin/env execthirdlinedocker.sh
--  info: use sed -i 's/\r//g' file if report "/usr/bin/env: ‘execthirdlinedocker.sh\r’: No such file or directory"
-- runghc    -i../transient/src -i../transient-universe/src -i../axiom/src   $1 ${2} ${3}

--  mkdir -p ./static && ghcjs --make   -i../transient/src -i../transient-universe/src  -i../axiom/src   $1 -o static/out && runghc   -i../transient/src -i../transient-universe/src -i../axiom/src   $1 ${2} ${3}


-- cd /projects/transient && cabal install -f debug --force-reinstalls && cd ../transient-universe && cabal install --force-reinstalls &&  runghc $1 $2 $3 $4


                     

{-# LANGUAGE ScopedTypeVariables, CPP, FlexibleInstances, FlexibleContexts, UndecidableInstances, RecordWildCards, MultiParamTypeClasses #-}


import Transient.Internals

import Transient.Move.Internals
import Transient.Move.Utils
import Transient.Logged
import Transient.Parse
import Control.Monad.State
import System.IO (hFlush,stdout) 
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Applicative
import System.Time
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
import Transient.Indeterminism
import Control.Exception hiding(onException)
import System.IO
import Control.Concurrent.MVar
#ifndef ghcjs_HOST_OS
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BSS
import Data.ByteString.Builder
import Data.String
import GHC.IO.Handle
#endif



main3 =  keep $ initNode $ inputNodes <|>  do
     local $ option "go"  "go"
     nodes <- local  getNodes
     
     r1 <- loggedc' $ wormhole (nodes !! 1) $  do
                 teleport 
                 
                 r <- localIO $ print "HELLO" >> return "WORLD"
                 teleport 
                 localIO $ print "WORLD" >> return r

     r2 <- wormhole  (nodes !! 1) $ loggedc $ do

                     teleport 
                     r <- local $ getSData <|>  return "NOOOOOO DAAATAAAAAA"
                     localIO $ print r
                     r <- loggedc $ do localIO $ print $ "HELLO22222" ++ r1;return $ "hello2" ++ r1      
                     teleport 
                     
                     return r
     localIO $ print $ "WORLD2222222" ++  r2    

     
main = keep $ initNode $ inputNodes <|>  do
     local $ option "go"  "go"
     nodes <- local getNodes
     t1 <- onAll $ liftIO $ getClockTime 
     r <- runAt (nodes !! 1) $  localIO $ BS.pack <$> replicateM 10000000  (randomRIO('a','z'))
     --localFix

     --localIO $ print $ BS.pack "RETURNED: " <> r
     t2 <- onAll $ liftIO $ getClockTime 
     localIO $ print  $ diffClockTimes  t2 t1
     
main2 = keep $ initNode $ inputNodes <|>  do
     local $ option "go"  "go"
     nodes <- local getNodes
     runAt (nodes !! 1) $ local $ do
           n <-  getMyNode
           handle <- liftIO $ openFile ("test"++ show (nodePort n)) AppendMode
           setData handle
      
     

     append  nodes  <|> close  nodes 
     
append  nodes  = do  
     local $ option "wr"  "write"

     runAt (nodes !! 1) $ local $ do
         handle <- getSData <|> error "no handle"
         liftIO $ hPutStrLn handle "hello"

close nodes  =  do
        local $ option  "cl" "close"

        

        runAt (nodes !! 1) $ local $ do
             handle <- getSData <|> error "no handle"
             liftIO $ hClose handle

     
     
     
     
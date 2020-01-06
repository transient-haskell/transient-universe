#!/usr/bin/env execthirdlinedocker.sh
--  info: use sed -i 's/\r//g' file if report "/usr/bin/env: ‘execthirdlinedocker.sh\r’: No such file or directory"
-- LIB="/projects" && runghc   -DDEBUG  -i${LIB}/transient/src -i${LIB}/transient-universe/src -i${LIB}/axiom/src   $1 ${2} ${3}

-- mkdir -p ./static && ghcjs --make   -i../transient/src -i../transient-universe/src  -i../axiom/src   $1 -o static/out && runghc   -i../transient/src -i../transient-universe/src -i../axiom/src   $1 ${2} ${3}


-- cd /projects/transient && cabal install -f debug --force-reinstalls && cd ../transient-universe && cabal install --force-reinstalls &&  runghc $1 $2 $3 $4

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Transient.Base
import Transient.Move
import Transient.Internals
import Transient.Move.Utils
import Control.Applicative
import Control.Monad.IO.Class
import Data.String
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Vector as V hiding (empty)
import Transient.MapReduce 
import Data.TCache
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Transient.EVars
import Data.Monoid
import Data.ByteString.Builder
import Transient.Parse
import Transient.Move.Services
import Transient.Indeterminism

-- to be executed with two or more nodes
main = keep $ initNode $ inputNodes <|> test9

test9= do
   local $ option "r" "run"
   atOtherNode $ localIO $ print "hello"
   where
   atOtherNode doit= do
     nodes <- local getNodes
     guard $ length nodes > 1
     runAt (nodes !! 1) doit 
test8 =  do
    --local $ option "r" "r"
    delData Serial
    n <- local getMyNode
    r <- (runAt n (local getMailbox) <> runAt n (local getMailbox) <> runAt n (local getMailbox)) <|> (local $ putMailbox "hello " >> empty) 
    -- r <- (return [3] <> (async (do print "here";return [5]:: IO [Int]) >> empty)) <|> liftIO (do print "here2"; return [7])
    localIO $ print (r  :: String)

--initNode $ inputNodes <|> test7


service= [("service","test suite")
         ,("executable", "test-transient1")
         ,("package","https://github.com/agocorona/transient-universe")]


test7= do 
    ins <- requestInstance service 1 
    localIO $ print ins

test6= do
    -- setData Parallel
    ((async getLine  >> return ())<> (liftIO $ print "world")) -- <|> liftIO (print "hello") 

test5= do
   -- option "r" "run"
   v1 <- liftIO $ newEmptyMVar

   setData Parallel
   (proc v1 <> proc2 ) <|> 
            (do  liftIO $ threadDelay 1000000 ; async $ putMVar v1 ("hello" :: String) )
   -- liftIO $ print (r :: String) 
   where
   proc2= liftIO $ print "world"
   proc v1=do
      --v <-  liftIO . atomically $ dupTChan v1
      liftIO $ print "PROC"
      
      (async $ do (readMVar v1) >>= print)
      
test3= do
    v <- newEVar
    -- option "r" "run"
    setData Parallel
    r <- (readEVar v <> readEVar v) <|> (do liftIO $ threadDelay 1000000; writeEVar v "hello" >> empty)
    liftIO $ print (r :: String)

test2=  do
  option "r" "run"
  setData Parallel
  r <- (async (return "1") ) <> (async (return "2")) <|> (do liftIO $ threadDelay 10000000;async (print "3") >> empty)

  --r <- (getMailbox <> getMailbox) <|> (do liftIO $ threadDelay 10000000; putMailbox (1 :: Int) >> empty)
  
  liftIO $ print (r :: String)
  
test12= do
    local $ option "r" "run"
    ns <- local getNodes
    r  <- runAt (ns !! 1) proc1 <> runAt (ns !! 2) proc2
    localIO $ print r
    where
    proc2= local $ return "hello from 3001"

    proc1= local $ do
            n <- getMyNode
            liftIO $ threadDelay 5000000
            return "hello from 3000" 
            
            
test1= do
    local $ option "r" "run"
    n <- local $ do ns <- getNodes; return $ ns !! 1
    localIO $ return () !> "RRRRRR"
    r <- (mclustered  (local getMailbox))  <|> do
                local $ option "s" "run"
                localIO $ return () !> "SSSSSS"
                runAt n $ local $ do 
                    putMailbox $ "hello "
                    empty

    localIO $ print (r :: String)
   
test= do
        let content= "hello world hello"
        local $ option "r" "run"  
        r <- reduce (+) $ mapKeyB (\w -> (w, 1 :: Int))  $ distribute $ V.fromList $ words content
   --     localIO $ print  ("MAP RESULT=", dds)
  --      -- local $ option "red" "reduce"
--        localIO $ getNodes >>= \n -> print ("NODES1", n)
    --    r <- reduce (+) $ DDS $ return dds
        localIO $ putStr "result:" >> print r

        localIO $ print "DONE"
        
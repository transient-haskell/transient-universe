#!/usr/bin/env ./execcluster.sh


{-# LANGUAGE CPP #-}
module Main where

#ifndef ghcjs_HOST_OS

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           GHC.Conc
import           Control.Applicative
import           Data.Monoid

import           Transient.Internals
import           Transient.Indeterminism
import           Transient.Logged
import           Transient.Move
import           Transient.Move.Utils
import           Transient.Move.Services
import           Transient.MapReduce
import           Transient.EVars
import           Control.Concurrent
import           System.IO.Unsafe
import           Data.List
import           Control.Exception.Base
import qualified Data.Map as M
import           System.Exit
import           System.Process

import Control.Monad.State
#define _UPK_(x) {-# UNPACK #-} !(x)


#define shouldRun(x) (local $ getMyNode >>= \n ->  assert (nodePort n == (nodePort x)) (return ()))
#define shouldRun1(x) (local $ getMyNode >>= \(Node _ p _ _) -> liftIO (print p >> print x >> print ( p == (x))))



main= do

 keep $ initNode $ do
          n1 <- local  getMyNode
          n2 <- requestInstall ""  ("executable", "TestSuite1")  !> "request"
          n3 <- requestInstall ""  ("executable", "TestSuite1")

--          shell "./TestSuite1 -p start/localhost/8081/add/localhost/8080/y"
--          shell "./TestSuite1 -p start/localhost/8082/add/localhost/8080/y"


          local $ option "f" "fire"
--           async $ do
--              let delay= (nodePort node -2000  + 1) *10000000
--              threadDelay delay

          nodes <- local getNodes
          onAll $ liftIO $ print nodes

          let  n1= head nodes
               n2= nodes !! 1
               n3= nodes !! 2



          localIO $ putStrLn "------checking Alternative distributed--------"
          r <- local $   do
                   runCloud $ (runAt n1 (shouldRun(n1) >> return "hello" ))
                         <|>  (runAt n2 (shouldRun(n2) >> return "world" ))
                         <|>  (runAt n3 (shouldRun(n3) >> return "world2" ))
          localIO $ print r

--          loggedc $ assert(sort r== ["hello", "world","world2"]) $ localIO $  print r

--          localIO $ putStrLn "--------------checking Applicative distributed--------"
--          r <- loggedc $(runAt n2000 (shouldRun(2000) >> return "hello "))
--                    <>  (runAt n2001 (shouldRun(2001) >> return "world " ))
--                    <>  (runAt n2002 (shouldRun(2002) >> return "world2" ))
--
--          assert(r== "hello world world2") $ localIO $ print r

--          localIO $ putStrLn "----------------checking monadic, distributed-------------"
--          r <- runAt n2000 (shouldRun(2000)
--                  >> runAt n2001 (shouldRun(2001)
--                       >> runAt n2002 (shouldRun(2002) >>  (return "HELLO" ))))
--
--          assert(r== "HELLO") $ localIO $ print r
--
--
--          localIO $ putStrLn "----------------checking map-reduce -------------"
--
--          r <- reduce  (+)  . mapKeyB (\w -> (w, 1 :: Int))  $ getText  words "hello world hello"
--          localIO $ putStr "SOLUTION: " >> print r
--          assert (sort (M.toList r) == sort [("hello",2::Int),("world",1)]) $ return r

--          local $ exit ()
--     print "SUCCESS"
--     exitSuccess


runNodes nodes= foldr (<|>) empty (map listen nodes) <|> return ()


#else

main= return ()
#endif

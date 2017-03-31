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
import           Transient.Base
import           Transient.Internals
import           Transient.Indeterminism
import           Transient.Logged
import           Transient.Move
import           Transient.MapReduce
import           Transient.EVars
import           Control.Concurrent
import           System.IO.Unsafe
import           Data.List
import           Control.Exception.Base
import qualified Data.Map as M
import           System.Exit


import Control.Monad.State
#define _UPK_(x) {-# UNPACK #-} !(x)


#define shouldRun(x) (local $ getMyNode >>= \(Node _ p _ _) -> assert ( p == (x)) (return ()))


main= do
     let numNodes = 3
         ports = [2000 .. 2000 + numNodes - 1]
         createLocalNode = createNode "localhost"
     nodes <- mapM createLocalNode ports
     let n2000= head nodes
         n2001= nodes !! 1
         n2002= nodes !! 2


     r <- keep' $  freeThreads $  runCloud $ do

          runNodes nodes

          localIO $ putStrLn "------checking Alternative distributed--------"
          r <- local $   collect 3 $
                   runCloud $ (runAt n2000 (shouldRun(2000) >> return "hello" ))
                         <|>  (runAt n2001 (shouldRun(2001) >> return "world" ))
                         <|>  (runAt n2002 (shouldRun(2002) >> return "world2" ))



          loggedc $ assert(sort r== ["hello", "world","world2"]) $ lliftIO $  print r

          lliftIO $ putStrLn "--------------checking Applicative distributed--------"
          r <- loggedc $(runAt n2000 (shouldRun(2000) >> return "hello "))
                    <>  (runAt n2001 (shouldRun(2001) >> return "world " ))
                    <>  (runAt n2002 (shouldRun(2002) >> return "world2" ))

          assert(r== "hello world world2") $ lliftIO $ print r

          lliftIO $ putStrLn "----------------checking monadic, distributed-------------"
          r <- runAt n2000 (shouldRun(2000)
                  >> runAt n2001 (shouldRun(2001)
                       >> runAt n2002 (shouldRun(2002) >>  (return "HELLO" ))))

          assert(r== "HELLO") $ lliftIO $ print r


          lliftIO $ putStrLn "----------------checking map-reduce -------------"

          r <- reduce  (+)  . mapKeyB (\w -> (w, 1 :: Int))  $ getText  words "hello world hello"
          lliftIO $ putStr "SOLUTION: " >> print r
          assert (sort (M.toList r) == sort [("hello",2::Int),("world",1)]) $ return r

          local $ exit ()
     print "SUCCESS"
     exitSuccess


runNodes nodes= foldr (<|>) empty (map listen nodes) <|> return ()


#else

main= return ()
#endif

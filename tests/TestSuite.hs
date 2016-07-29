{-# LANGUAGE CPP #-}
module Main where

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
import           Transient.Stream.Resource
import           Transient.MapReduce
import           Transient.EVars
import Control.Concurrent
import System.IO.Unsafe
import Data.List
import Control.Exception.Base
import qualified Data.Map as M
import System.Exit

#define _UPK_(x) {-# UNPACK #-} !(x)


#define shouldRun(x) (local $ getMyNode >>= \(Node _ p _ _) -> assert ( p == (x)) (return ()))


main= do
     let numNodes = 4
         ports = [2000 .. 2000 + numNodes - 1]
         createLocalNode = createNode "localhost"
         nodes = map createLocalNode ports
         n2000= head nodes
         n2001= nodes !! 1
         n2002= nodes !! 2
         n2003= nodes !! 3
     r <-runCloudIO $ do
          runNodes nodes
          local $ option "s" "start"

          local $ do
              liftIO $ putStrLn "--------------checking  parallel execution, events --------"
              ev <- newEVar
              r <- collect 3 $ readEVar ev <|> ((choose [1..3] >>= writeEVar ev) >> stop)

              assert (sort r== [1,2,3]) $ liftIO $ print r

          lliftIO $ putStrLn "--------------checking Applicative distributed--------"
          r <- loggedc $(runAt n2000 (shouldRun(2000) >> return "hello "))
                    <>  (runAt n2001 (shouldRun(2001) >> return "world " ))
                    <>  (runAt n2002 (shouldRun (2002) >> return "world2" ))
          localIO $ print r
          assert(r== "hello world world2") $ lliftIO $ print r



          lliftIO $ putStrLn "------checking Alternative distributed--------"
          r <- local $ collect' 3 1 0 $
                   runCloud $ (runAt n2000 (shouldRun(2000) >> return "hello"))
                         <|>  (runAt n2001 (shouldRun(2001) >> return "world" ))
                         <|>  (runAt n2002 (shouldRun(2002) >> return "world2" ))

          loggedc $  assert(sort r== ["hello", "world","world2"]) $ lliftIO $  print r





          lliftIO $ putStrLn "----------------checking monadic, distributed-------------"
          r <- runAt n2000 (shouldRun(2000)
                  >> runAt n2001 (shouldRun(2001)
                       >> runAt n2002 (shouldRun(2002) >>  (return "HELLO" ))))

          assert(r== "HELLO") $ lliftIO $ print r


          lliftIO $ putStrLn "----------------checking map-reduce -------------"
          r <- reduce  (+)  . mapKeyB (\w -> (w, 1 :: Int))  $ getText  words "hello world hello hi"
          lliftIO $ putStr "SOLUTION: " >> print r
          assert (sort (M.toList r) == sort [("hello",2::Int),("hi",1),("world",1)]) $ return r



          local $ exit ()




          lliftIO $ print "SUCCES"

     exitSuccess

--getEffects :: Loggable a =>  Cloud [(Node, a)]
--getEffects=lliftIO $ readIORef effects
--
runNodes nodes= foldl (<|>) empty (map listen nodes) <|> return()
--
--
--delEffects= lliftIO $ writeIORef effects []
--effects= unsafePerformIO $ newIORef []
--
--EFFECT x= do
--   node <- onAll getMyNode
--   lliftIO $ atomicModifyIORef effects $ \ xs -> ((node,x): xs,())
--   return()





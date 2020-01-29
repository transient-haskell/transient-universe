{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Main where

#ifndef ghcjs_HOST_OS

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Applicative
import           Data.Monoid
import           Transient.Base
import           Transient.Internals
import           Transient.Indeterminism
import           Transient.Move.Internals
import           Transient.Move.Utils
import           Transient.Move.Services
import           Transient.MapReduce
import           Data.List
import qualified Data.Map as M
import           System.Exit
import           Control.Monad.State
import           Control.Exception

import           Control.Concurrent(threadDelay )

-- #define _UPK_(x) {-# UNPACK #-} !(x)


SHOULDRUNIN x=  local $ getMyNode >>= \p ->  assert ( p == (x)) (liftIO $ print p)

service= [("service","test suite")
         ,("executable", "test-transient1")
         ,("package","https://github.com/agocorona/transient-universe")]

main= do
     mr <- keep test
     endMonitor 

     case mr of
       Nothing -> print "NO RESULT, NO THREADS RUNNING" >> exitFailure
       Just Nothing -> print "SUCCESS" >> exitSuccess 
       Just (Just e) -> putStr "FAIL: " >> print e >> exitFailure

 

      

test=  initNodeServ service  "localhost" 8080 $ do
          node0 <- local getMyNode
          
          local $ guard (nodePort node0== 8080)       -- only executes in node 8080

          [node1, node2] <- requestInstance service 2 

          local ( option "f" "fire")   <|> return ""       -- to repeat the tests,  remove the "exit" at the end 



          localIO $ putStrLn "------checking  empty in remote node when the remote call back the caller #46 --------"
          r <- runAt node1 $ do
               SHOULDRUNIN node1
               runAt node2 $  (runAt node1 $ SHOULDRUNIN node1 >> empty ) <|>  (SHOULDRUNIN node2 >> return "world")
          localIO $ print r
          

          localIO $ putStrLn "------checking Alternative distributed--------"
          r <- local $   collect 3 $
                   runCloud $ (runAt node0 (SHOULDRUNIN( node0) >> return "hello" ))
                         <|>  (runAt node1 (SHOULDRUNIN( node1) >> return "world" ))
                         <|>  (runAt node2 (SHOULDRUNIN( node2) >> return "world2" ))

          assert(sort r== ["hello", "world","world2"]) $ localIO $  print r         
          
          localIO $ putStrLn "--------------checking Applicative distributed--------"
          r <- loggedc $(runAt node0 (SHOULDRUNIN( node0) >> return "hello "))
                    <>  (runAt node1 (SHOULDRUNIN( node1) >> return "world " ))
                    <>  (runAt node2 (SHOULDRUNIN( node2) >> return "world2" ))

          assert(r== "hello world world2") $ localIO $ print r

          localIO $ putStrLn "----------------checking monadic, distributed-------------"
          r <- runAt node0 (SHOULDRUNIN(node0)
                  >> runAt node1 (SHOULDRUNIN(node1)
                       >> runAt node2 (SHOULDRUNIN(node2) >>  (return "HELLO" ))))

          assert(r== "HELLO") $ localIO $ print r
 
          localIO $ putStrLn "----------------checking map-reduce -------------"

          r <- reduce  (+)  . mapKeyB (\w -> (w, 1 :: Int))  $ getText  words "hello world hello"
          localIO $  print r
          assert (sort (M.toList r) == sort [("hello",2::Int),("world",1)]) $ return r
          

          local $ exit (Nothing  :: Maybe SomeException) -- remove this to repeat the test
 


#else
main= return ()
#endif

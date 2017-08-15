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
import           Transient.Move
import           Transient.Move.Utils
import           Transient.Move.Services
import           Transient.MapReduce
import           Data.List
import qualified Data.Map as M
import           System.Exit
import           Control.Monad.State
import           Control.Exception

-- #define _UPK_(x) {-# UNPACK #-} !(x)


#define shouldRun(x) (local $ getMyNode >>= \p -> assert ( p == (x)) (return ()))


service= [("service","test suite")
         ,("executable", "test-transient1")
         ,("package","https://github.com/agocorona/transient-universe")]

main= do
     mr <- keep $ test   `catcht` \(e:: SomeException) -> liftIO (putStr "EXCEPTiON: " >> print e) >> exit (Just e)
     case mr of
       Nothing -> print "NO RESULT, NO THREADS RUNNING" >> exitFailure
       Just Nothing -> print "SUCCESS" >> exitSuccess 
       Just (Just e) -> putStr "FAIL: " >> print e >> exitFailure


test=  initNodeServ service  "localhost" 8080 $ do
     
          
          node0 <- local getMyNode
          
          local $ guard (nodePort node0== 8080)       -- only executes in node 8080

          
      --  local $ option "get" "get instances"

          localIO $ return () !> "REQUEST INSTANCE "
        
          [node1, node2] <- requestInstance "PIN1" service 2
        --   localIO $ putStrLn "------checking connect --------"
        --   connect' node1
        --   connect' node2
        --   runAt node1 $ local $ do
        --        nodes <- getNodes
        --        assert (length nodes== 3) return ()
 

          local ( option "f" "fire")   <|> return ""       -- to repeat the test,  remove exit


          localIO $ putStrLn "------checking Alternative distributed--------"
          r <- local $   collect 3 $
                   runCloud $ (runAt node0 (shouldRun( node0) >> return "hello" ))
                         <|>  (runAt node1 (shouldRun( node1) >> return "world" ))
                         <|>  (runAt node2 (shouldRun( node2) >> return "world2" ))

          assert(sort r== ["hello", "world","world2"]) $ localIO $  print r
          
          localIO $ putStrLn "--------------checking Applicative distributed--------"
          r <- loggedc $(runAt node0 (shouldRun( node0) >> return "hello "))
                    <>  (runAt node1 (shouldRun( node1) >> return "world " ))
                    <>  (runAt node2 (shouldRun( node2) >> return "world2" ))

          assert(r== "hello world world2") $ localIO $ print r

          localIO $ putStrLn "----------------checking monadic, distributed-------------"
          r <- runAt node0 (shouldRun(node0)
                  >> runAt node1 (shouldRun (node1)
                       >> runAt node2 (shouldRun(node2) >>  (return "HELLO" ))))

          assert(r== "HELLO") $ localIO $ print r
 
          localIO $ putStrLn "----------------checking map-reduce -------------"

          r <- reduce  (+)  . mapKeyB (\w -> (w, 1 :: Int))  $ getText  words "hello world hello"
          localIO $  print r
          assert (sort (M.toList r) == sort [("hello",2::Int),("world",1)]) $ return r
          
          localIO endMonitor

          local $ exit (Nothing  :: Maybe SomeException) -- remove this to repeat the test
 


#else
main= return ()
#endif

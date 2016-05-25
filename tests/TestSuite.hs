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

main= do
     let numNodes = 3
         ports = [2000 .. 2000 + numNodes - 1]
         createLocalNode = createNode "localhost"
         nodes = map createLocalNode ports
         n2000= head nodes
         n2001= nodes !! 1
         n2002= nodes !! 2

     r <-runCloudIO $ do

          local $ addNodes nodes

          runNodes nodes
          local $ option "s" "start"

--          local $ do
--
--              ev <- newEVar
--              r <- collect 3 $ readEVar ev <|> ((choose [1..3] >>= writeEVar ev) >> stop)
--
--              assert (sort r== [1,2,3]) $ liftIO $ print r


--          lliftIO $ putStrLn "--------------checking Applicative distributed--------"
--          r <-  (runAt n2000 (effect "2000" >> return "hello "))
--                    <>  (runAt n2001 (effect "2001" >> return "world " ))
--                    <>  (runAt n2002 (effect "2002" >> return "world2" ))
--          assert(r== "hello world world2") $ lliftIO $ print r
--
--          effs <- getEffects  :: Cloud [(Node, String)]
--          lliftIO $ print effs
--          assert (sort effs == sort [(n2000,"2000"),(n2001,"2001"),(n2002,"2002")]) $return ()
--          delEffects




--          lliftIO $ putStrLn "------checking Alternative distributed--------"
--          r <- local $ collect' 3 1 0  $ runCloud $ (runAt n2000 (effect "2000" >> return "hello"))
--                    <|>  (runAt n2001 (effect "2001" >> return "world" ))
--                    <|>  (runAt n2002 (effect "2002" >> return "world2" ))
--
--          loggedc $ do
--            assert(sort r== ["hello", "world","world2"]) $ lliftIO $  print r
--            effs <- getEffects ::  Cloud [(Node, String)]
--            lliftIO $ print effs
--            assert (sort effs == sort [(n2000,"2000"),(n2001,"2001"),(n2002,"2002")]) $return ()
--            delEffects




--          lliftIO $ putStrLn ">>>>>>>>>>>>checking monadic, distributed<<<<<<<<<<<<<<<"
--          r <- runAt n2000 (effect "n2000"
--                  >> runAt n2001 (effect "n2001"
--                       >> runAt n2002 (effect "n2002" >>  (return "HELLO" ))))
--
--          assert(r== "HELLO") $ lliftIO $ print r
--
--          effs <- getEffects
--          assert (sort effs == sort [(n2000,"n2000"),(n2001,"n2001"),(n2002,"n2002")]) $ return ()
--          delEffects






          r <- reduce  (+)  . mapKeyB (\w -> (w, 1 :: Int))  $ getText  words "hello world hello hi"
          lliftIO $ putStr "SOLUTION: " >> print r
          assert (sort (M.toList r) == sort [("hello",2),("hi",1),("world",1)]) $ return ()



          local $ exit ()




          lliftIO $ print "SUCCES"

     exitSuccess

getEffects :: Loggable a =>  Cloud [(Node, a)]
getEffects=lliftIO $ readMVar effects

runNodes nodes= foldl (<|>) empty (map listen nodes) <|> return()


delEffects= lliftIO $ modifyMVar_ effects $ const $ return[]
effects= unsafePerformIO $ newMVar []

effect x= do
   node <- local getMyNode
   lliftIO $ modifyMVar_ effects $ \ xs ->  return $ (node,x): xs
   return()



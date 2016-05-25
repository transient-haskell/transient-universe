{-# LANGUAGE   CPP   #-}

module Main where

import Prelude hiding (div,id)
import Transient.Internals
import Transient.Base


{-
#ifdef ghcjs_HOST_OS
   hiding ( option)
#endif
import GHCJS.HPlay.Cell
import GHCJS.HPlay.View
#ifdef ghcjs_HOST_OS
   hiding (map,input)
#else
   hiding (map, option,input)
#endif
-}

import Transient.Move

import Control.Applicative
import Control.Monad
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Map as M
import Transient.MapReduce
import Transient.Logged
import Control.Monad.IO.Class
import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO.Unsafe
import Data.Monoid

import Transient.EVars
import Transient.Indeterminism
import Transient.Internals
import Data.List

import Control.Exception.Base
import qualified Data.Vector as V
import Data.IORef

runNodes nodes= foldl (<|>) empty (map listen nodes) <|> return()



main= runCloudIO $ do
     port <- local getPort
     let node= createNode "localhost" port

     listen node <|> return ()

     addNode <|> return()
--     ev <- onAll newEVar
     local$   option "run" "run the program"



     nodes <- local getNodes

     let node0= nodes !! 0
         node1= nodes !! 1
         node2= nodes !! 2
     lliftIO $ print $  nodes
     foldr (<|>) empty $ map (\node -> runAt node  $ lliftIO $ print "hello" )  $  nodes
--     empty <|> runAt node0 ( lliftIO $ print "hello") <|>
--       runAt node1 ( lliftIO $ print "hello") <|>
--         runAt node2 ( lliftIO $ print "hello")

--       oneThread $ return ()

--     wormhole node1 $ do
--
--        atRemote $  lliftIO $ print "IN THE OTHER"
--
--        r <- local  (return 2  !>  ("executing", 2))
--
--        r <- atRemote  $ do
--                  lliftIO $ print "IN THE OTHER AGAIN"
--                  return $  "hello " ++  show (r:: Int)
--
--        lliftIO $ print r


--       r <- showTh <|>( waitNodeEvents >>= \x -> liftIO (print (x :: String))) <|>
--                  (waitNodeEvents >>= \x -> liftIO (print (x :: Int)))  <|>
--             do
--                                      r <-  option "s" "send"
--                                      sendNodeEvent "hello"
--                                      stop
--
--       cleanNodeEvents
--       r <- showTh <|> option "a" "a" <|> option "b" "b"
--       liftIO $ print (r :: String)

showTh= option "th" "show threads" >> showThreads >> stop

--clean= do
--  option "clean" "clean the evar"
--single :: run once
--bottleneck True :: oneThread
--     r <- runAt node1 (local $ async $ return1 "hello") <|> runAt node2 (local $ async $ return1 "world")
--
--     r <- receiveAtNodes nodes <|> sendFromNodes nodes :: Cloud ()
--
--     lliftIO $ print ("result", r)
--
--return1 x= liftIO(threadDelay 10000) >> return x
--
--async1 x= async x <*** setData WasRemote
--
--sendFromNodes nodes node = runAt node $ do
--      clustered' nodes $ local $ liftIO (threadDelay 1000000) >>  putMailBox "box" "hello"
--      empty
--
--receiveAtNodes nodes = clustered' $ nodes $  do
--                   r<- local $ getMailBox "box"  !> "GETMAILBOX"  :: Cloud String
--                   lliftIO $ print ("received",r)
--                   empty
--
--clustered'  nodes f= foldr (<|>) empty $ map (\n -> runAt n f ) nodes

--     r <- onAll $ runCloud(runAt node1 (local $ option "f" "fire" <|> return "ready"))
--                    <|* (runCloud (lliftIO (print "triggered") >>stop))

--     r <- reduce  (+) . mapKeyB (\w -> (w, 1 :: Int))
--                               $ getText  words "asad asd asd"
--     lliftIO $ putStr "--------------->" >>  print r



--     let return1 x=  threadDelay 100000 >> return x
--     r <- mparallelize (\n -> runAt n (return "hello"))  nodes
--     r <- mclustered $ return "hello"

--     r <- mempty <> runAt node0 (return "hello")  <> runAt node1 (return "world ")  <> runAt node2 (return "world2 ")
--     r <- (local $ getMailBox "box") <|>
--           (parallelize (sendbox node0) nodes >> empty)
--     wormhole node1 $ do
--       r <- local $ option "l" "left" <|> option "r" "right"
--       r1 <- if r == "l" then atRemote  $ do   local $ return ()
--                                               r <- do
--                                                  local $ threads 1 $ do
--                                                  r <-choose[1..3::Int]
--                                                  liftIO $ threadDelay 10000000
--                                                  return r
--                                               local $ return ()
--                                               return r
--             else do
--               local $ return ()
--               atRemote $ do lliftIO $ print "right"; return 0
--       lliftIO $ print r1
--
--sendbox node0 n=  runAt n $  runAt node0 (local $ putMailBox "box"( "*****hello" ++ show n))
--
--
--mparallelize f xs =  foldr (<>) mempty $ map f xs
--
--content= "hello world hello hi"
--

--     r <- (runAt(nodes !! 1) $ return "hello")
--             <> (runAt (nodes !! 2) $ return " world")


--
--
--     stop
--
--     wormhole node $   sendMessages  <|> waitMessages



getPort :: TransIO Integer
getPort =
       do
          oneThread $ option "start" "re/start"
          port <- input (const True) "port to listen? "
          liftIO $ putStrLn "node started"
          return port

addNode=do
   onServer $ do
          local $ option "add"  "add a new node at any moment"

          host <- local $ do
                    r <- input (const True) "Host to connect to: (none): "
                    if r ==  "" then stop else return r

          port <-  local $ input (const True) "port?"

          connectit <- local $ input (\x -> x=="y" || x== "n") "connect to get his list of nodes?"
          let nnode= createNode host port
          if connectit== "y" then connect'  nnode
                             else local $ addNodes [nnode]
   empty


-- only execute if is the browser but can call the server:
onBrowser x= do
     r <- local $  return isBrowserInstance
     if r then x else empty

onServer x= do
     r <- local $  return isBrowserInstance
     if not r then x else empty


--
--single :: one single instance
--
--last :: la nueva instance elimina a la anterior
--
--
singletons :: MVar (M.Map Int ThreadId)
singletons = unsafePerformIO $ newMVar M.empty

single :: TransIO a -> TransIO a
single  mx=  Transient $ do
     id <- genId
     b <- liftIO $ modifyMVar singletons $ \ map -> case M.lookup id map of

                     Nothing -> do
                         th <- myThreadId
                         return (M.insert id th map,False)
                     Just th -> return (map, True)

     case b of
       False -> runTrans mx
       True -> return Nothing

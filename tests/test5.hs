module Main where

import Transient.Move
import Transient.Logged
import Transient.Base
import Transient.Internals
import Transient.Internals((!>))
import Transient.Indeterminism
import Transient.EVars
import Network
import Control.Applicative

import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Data.Monoid
import System.IO
import Control.Monad
import Data.Maybe
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Typeable
import Data.IORef
import Data.List((\\))






-- to be executed with two or more nodes
main = do
--     args <- getArgs
--     let hostname= read $ head args :: String
--         remotehost= read $ args !! 1
--         port= 2000
--         numNodes = 2
--         ports = [2000 .. 2000 + numNodes - 1]

--         nodes = (createNode "localhost" ports
--         node1= head nodes
--         node2= nodes !! 1
     let node= createNode "localhost" 2000
     runCloudIO $ do
         listen node <|> return ()

         local $ option "s" "start"

         box <- local newMailBox  !> "NEWMAILBOX"

         (runAt node $ local $ do
                  getMailBox (box !> ("get1",box)) >>= \x -> do
                  cleanMailBox box  ""   !> "clean1"
                  liftIO $ putStrLn x
                  return SDone)
          <|> (runAt node $ local $ do
                  getMailBox (box !> ("get1",box)) >>= \x -> do
                  cleanMailBox box  ""   !> "clean2"
                  liftIO $ putStrLn x
                  return SDone)
          <|> (runAt node $ local $ do
                  getMailBox (box !> ("get1",box)) >>= \x -> do
                  cleanMailBox box  ""   !> "clean3"
                  liftIO $ putStrLn x
                  return SDone)

          <|> (runAt node $ local $ putMailBox (box !> ("put",box)) "hello" >> return (SMore ""))



runNodes nodes= foldl (<|>) empty (map listen nodes) <|> return()



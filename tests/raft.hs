module Transient.Raft where

import Control.Applicative
import Data.Monoid
import Control.Monad.IO.Class
import Transient.Internals
import Transient.Indeterminism
import Transient.Move
import Transient.Move.Services
import System.IO.Unsafe
import Data.IORef
import Control.Concurrent(threadDelay)
import Data.Maybe
import System.Random

rmaster = unsafePerformIO $ newIORef Nothing

heartbeatTimeout= 10000000 :: Int


cunique= local . unique . runCloud

heartBeat raftNodes = cunique $ do
   localIO $ do
       threadDelay heartbeatTimeout
       atomicModifyIORef rmaster $ const (Nothing,())
   election raftNodes

raft raftNodes  request= do
  master <- localIO $ readIORef rmaster
  if isNothing master
    then election  raftNodes >> raft raftNodes request
    else do
       node <- local getMyNode
       if master== Just node then process raftNodes request >>= return . Right
       else return $ Left master

process raftNodes request= do
  let half=  length raftNodes` div` 2 :: Int
  resps <- local $ collect' half 0.1 (fromIntegral heartbeatTimeout)
             $  runCloud $ cluster raftNodes request

  if length resps > half then return resps else empty

election raftNodes= cunique $ do

  sentVote  <- onAll . liftIO $ newIORef False !> "election"

  timeoutElection <- localIO $ randomRIO (150, 300)
  localIO $ threadDelay timeoutElection

  votes <- mcluster raftNodes . localIO $ atomicModifyIORef sentVote $ \v -> (not v, [v])

  let nvotes = length $ filter (==True) votes
  if nvotes > length raftNodes `div` 2
   then do
       node <- local getMyNode
       cluster raftNodes . localIO $ atomicModifyIORef rmaster $ const (Just node,())
       heartBeat raftNodes
   else do
       localIO $ atomicModifyIORef sentVote $ const (False,())
       election raftNodes

cluster nodes proc= callNodes' (<|>) empty nodes proc
mcluster nodes proc= callNodes' (<>) mempty nodes proc

callNodes' op init nodes proc= foldr op init $ map (\node -> runAt node proc) nodes

runRaftNodes ports= do
    nodes <- onAll $  mapM (\p -> liftIO $ createNodeServ "localhost" p [("raft","raft")]) ports
    foldl (<|>) empty (map listen nodes) <|> return()



main= keep $ runCloud $ do
     runRaftNodes [4000..4005]
     raftNodes <- local getNodes
     local $ option "input" "input"
     msg <- local $ input (const  True) "enter a message >"
     r <- raft raftNodes . local $ do
                      node <- getMyNode
                      liftIO $ do
                             putStr "request EXECUTED at node: "
                             print node
                             print msg
                             return msg
                :: Cloud (Either (Maybe Node) [String])
     localIO $ do putStr "response from the cluster: "; print r



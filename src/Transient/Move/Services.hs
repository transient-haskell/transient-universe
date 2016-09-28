-----------------------------------------------------------------------------
--
-- Module      :  Transient.Move.Services
-- Copyright   :
-- License     :  GPL-3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Transient.Move.Services  where

import Transient.Base
import Transient.Move
import Transient.Logged(Loggable(..))
import Transient.Backtrack
import Transient.Internals  (RemoteStatus(..), Log(..))
import Transient.Move.Utils

import Transient.EVars
import Transient.Indeterminism
import Control.Monad.IO.Class
import System.Process
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Applicative

import System.Directory
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Concurrent(threadDelay)
import Control.Exception
import Data.IORef

monitorService= ("https://github.com/agocorona/transient-universe","monitor")


install :: String  -> String -> String -> Int -> IO ()
install package program host port =  do
     exist <-  findExecutable program -- liftIO $ doesDirectoryExist  packagename
     when (isNothing exist) $ do
         let packagename = name package
         when (null packagename) $ error $ "source for \""++package ++ "\" not found"
         callProcess  "git" ["clone",package]
         liftIO $ putStr package >> putStrLn " cloned"
         setCurrentDirectory packagename
         callProcess  "cabal" ["install","--force-reinstalls"]
         setCurrentDirectory ".."
         return()
     let prog = pathExe  program host port
     print $ "executing "++ prog
     let createprostruct= shell prog
     createProcess $ createprostruct ; return ()

     threadDelay 2000000

     return()                           --  !> ("INSTALLED", program)
     where
     pathExe  program host port=  program  ++ " -p start/" ++ show host ++"/" ++ show port


name url=  slash . slash . slash $ slash url
  where
  slash= tail1 . dropWhile (/='/')
  tail1 []=[]
  tail1 x= tail x

monitorPort= 3000
rfreePort :: MVar Int
rfreePort = unsafePerformIO $ newMVar  (monitorPort +1)

freePort :: MonadIO m => m Int
freePort= liftIO $ modifyMVar rfreePort $ \ n -> return (n+1,n)

initService ident service@(package, program)=
    (local $ findInNodes service >>= return . head) <|> requestInstall service
    where
    requestInstall service =  do
       mnode <- callService' ident monitorNode (ident,service)
       case mnode of
         Nothing -> empty
         Just node -> do
               local $ addNodes [node]      -- !> ("ADDNODES",service)
               return node

startMonitor=  do
        createProcess . shell $ "monitorService -p start/"++ show monitorPort
        threadDelay 2000000


nodeService (Node h _ _ _) port service=  do
      pool <- newMVar []
      return $ Node h port pool [service]

findInNodes service =  do
      nodes <-  getNodes
      let ns = filter (\node  -> service `elem` nodeServices node) nodes
      if null ns then empty
                 else return ns



-- where
--
-- callNodes' op init proc= loggedc $ do
--    nodes <-  local getNodes
--    let nodes' = filter (not . isWebNode) nodes
--    foldr op init $ map (\node -> runAt node $ proc node) nodes'  :: Cloud [Node]
--    where
--    isWebNode Node {nodeServices=srvs}
--         | ("webnode","") `elem` srvs = True
--         | otherwise = False


rfriends        =   unsafePerformIO $ newIORef ([] ::[String])
rservices       =   unsafePerformIO $ newIORef ([] ::[Service])
ridentsBanned   =   unsafePerformIO $ newIORef ([] ::[String])
rServicesBanned =   unsafePerformIO $ newIORef ([] ::[Service])

inputAuthorizations= do
    oneThread $ option "authorizations" "authorizations"
    showPerm <|> friends <|> services <|> identBanned <|> servicesBanned
    empty

    where
    friends= do
      option "friends" "friendsss"
      fr <- input (const True) "enter the friend list: "
      liftIO $ writeIORef rfriends (fr :: [String])

    services= do
      option "services" "services"
      serv <- input (const True) "enter service list: "
      liftIO $ writeIORef rservices (serv :: [Service])

    identBanned= do
      option "bannedIds"  "banned users"
      ban <- input (const True) "enter the users banned: "
      liftIO $ writeIORef ridentsBanned (ban ::[String ])
      rs <- liftIO $ readIORef ridentsBanned
      liftIO $ print rs

    servicesBanned= do
      option "bannedServ"  "banned services"
      ban <- input (const True) "enter the services banned: "
      liftIO $ writeIORef rServicesBanned (ban :: [Service])

    showPerm= do
     option "show"  "show permissions"
     friends            <- liftIO $ readIORef rfriends
     services           <- liftIO $ readIORef rservices
     identsBanned       <- liftIO $ readIORef ridentsBanned
     servicesBanned     <- liftIO $ readIORef rServicesBanned
     liftIO $ putStr "allowed:          " >> print friends
     liftIO $ putStr "banned:           " >> print identsBanned
     liftIO $ putStr "services allowed: " >> print services
     liftIO $ putStr "services banned:  " >> print servicesBanned


authorizeService :: MonadIO m => String -> Service -> m Bool
authorizeService ident service=   do

     friends            <- liftIO $ readIORef rfriends
     services           <- liftIO $ readIORef rservices
     identsBanned       <- liftIO $ readIORef ridentsBanned
     servicesBanned     <- liftIO $ readIORef rServicesBanned

     return $ if (null friends || ident `elem` friends)
        && (null services || service `elem` services)
        && (null identsBanned || ident `notElem` identsBanned)
        && (null servicesBanned || service `notElem` servicesBanned)
      then True  else False
  where
  notElem a b= not $ elem a b


callService
    :: (Loggable a, Loggable b)
    => String -> Service -> a  -> Cloud b
callService ident service params = do
    node <-  initService ident service     --  !> ("callservice initservice", service)
    callService' ident node params         -- !>  ("NODE FOR SERVICE",node)

monitorNode= unsafePerformIO $ createNodeServ "localhost"
            (fromIntegral monitorPort)
            [monitorService]

callService' ident node params = do

    onAll $ onFinish (\me -> do
                case fmap fromException me  :: Maybe(Maybe IOException) of
                  Nothing -> return ()
                  Just (Just e') -> do
                      noFinish
                      liftIO startMonitor)
    log <- onAll $ do
             log  <- getSData <|> return emptyLog
             setData emptyLog
             return log

    r <- wormhole node $  do
             local $ return params
             teleport
             local empty

    restoreLog log                         -- !> "RESTORELOG"

    return  r
    where
    restoreLog (Log _ _ logw)= onAll $ do
       Log _ _ logw' <- getSData <|> return emptyLog

       let newlog= reverse logw' ++ logw
--       return ()                 !> ("newlog", logw,logw')
       setData $ Log False newlog newlog

    emptyLog= Log False [] []



runEmbeddedService :: (Loggable a, Loggable b) =>  Service -> (a -> Cloud b) -> Cloud b
runEmbeddedService servname serv =  do
   node <- localIO $ do
          port <- freePort
          createNodeServ "localhost" (fromIntegral port) [servname]
   listen node
   wormhole notused $ loggedc $ do
      x <- local $ return notused
      r <- onAll $ runCloud (serv x) <** setData WasRemote
      local $ return r
      teleport
      return r

  where

  notused= error "runEmbeddedService: variable should not be used"

runService :: (Loggable a, Loggable b) =>  Service -> (a -> Cloud b) -> Cloud b
runService servname serv =  do
   initNodeServ [servname]
   service
--   onAll inputAuthorizations   -- <|> inputNodes
   where
   service=
       wormhole (notused 1) $  do
          x <- local $ return $ notused 2
          setData emptyLog
          r <- local $ runCloud (serv x) <** setData WasRemote
          teleport
          return r

   emptyLog= Log False [] []
   notused n= error $  "runService: "++ show (n::Int) ++ " variable should not be used"
   initNodeServ servs=do
      mynode <- local  getNode

      local $ do
         conn <- defConnection
         setData  conn{myNode = mynode}
      onAll inputAuthorizations <|> (inputNodes >> empty) <|> return ()
      listen mynode
      where
      getNode :: TransIO Node
      getNode =  if isBrowserInstance then liftIO createWebNode else do
          oneThread $ option "start" "re/start node"
          host <- input (const True) "hostname of this node (must be reachable): "
          port <- input (const True) "port to listen? "
          liftIO $ createNodeServ host port servs

      inputNodes= do
           onServer $ do
                  local $ option "add"  "add a new monitor node"

                  host <- local $ do
                            r <- input (const True) "Host to connect to: (none): "
                            if r ==  "" then stop else return r

                  port <-  local $ input (const True) "port? "

                  nnode <- localIO $ createNodeServ host port [monitorService]
                  local $ do
                                       liftIO $ putStr "Added node: ">> print nnode
                                       addNodes [nnode]
           empty

{- |
a service called monitor:
  runService
  receive request for a service.
  check service in list
  if executing return node
  when not installed install
  execute
  return node
-}







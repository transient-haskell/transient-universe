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
import Transient.Internals(RemoteStatus(..), Log(..))
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
--import Data.TCache hiding(onNothing)


pathExe package program port= {-"./"++package++"/dist/build/"++package++"/"++  -} program
                                       ++ " -p start/" ++ show port

install :: String  -> String -> Int -> Cloud ()
install package program port =  do
     exist <- localIO $ findExecutable program -- liftIO $ doesDirectoryExist  packagename
     let packagename = name package
     when (isNothing exist) $ local $ liftIO $ do

         when (null packagename) $ error $ "source for \""++package ++ "\" not found"
         callProcess  "git" ["clone",package]
         liftIO $ putStr package >> putStrLn " cloned"
         setCurrentDirectory packagename
         callProcess  "cabal" ["install","--force-reinstalls"]
         setCurrentDirectory ".."
         return()
     let prog = pathExe packagename program port
     lliftIO $ print $ "executing "++ prog
     localIO $ do createProcess $ shell prog ; return ()  -- ) <|> return ()


     return() -- !> "INSTALLED"

name url=  slash . slash . slash $ slash url
  where
  slash= tail1 . dropWhile (/='/')
  tail1 []=[]
  tail1 x= tail x

rfreePort :: MVar Int
rfreePort = unsafePerformIO $ newMVar  3000

freePort :: MonadIO m => m Int
freePort= liftIO $ modifyMVar rfreePort $ \ n -> return (n+1,n)

initService ident service@(package, program)= loggedc $ do
    nodes <- local getNodes
    case find (\node  -> service `elem` nodeServices node) nodes  of
       Just node -> return node                 -- !> "found"
       Nothing -> do

          node <- runAt (head nodes)  $ do
                    thisNode <- local getMyNode
                    yn<- requestService ident service
                    if yn   then do
                        port <- onAll freePort
                        localIO $ putStr "installing " >> putStrLn package
                        install package program  port
                        nodeService thisNode port
                      else empty
          local $ addNodes [node]
          return node
    where
    nodeService (Node h _ _ _) port= localIO $ do
       pool <- newMVar []
       return $ Node h port pool [service]

callOne :: Loggable a =>  Cloud a ->  Cloud a
callOne = callNodes after empty

after mx my= waitone mx <|> my
  where
  waitone mx = local $ do
       rs <- collect' 1 1 0 $ runCloud mx
       return $ head rs

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


rfriends        =   unsafePerformIO $ newMVar []
rservices       =   unsafePerformIO $ newMVar []
ridentsBanned   =   unsafePerformIO $ newMVar []
rServicesBanned =   unsafePerformIO $ newMVar []

requestService ident service= local $  do
     friends            <- liftIO $ readMVar rfriends
     services           <- liftIO $ readMVar rservices
     identsBanned       <- liftIO $ readMVar ridentsBanned
     servicesBanned     <- liftIO $ readMVar rServicesBanned

     return $ if (null friends || ident `elem` friends)
        && (null services || service `elem` services)
        && (null identsBanned || ident `notElem` identsBanned)
        && (null servicesBanned || service `notElem` servicesBanned)
      then True else False
  where
  notElem a b= not $ elem a b


callService
    :: (Loggable a, Loggable b)
    => String -> Service -> a  -> Cloud b
callService ident service params = do

    node <-  initService ident service


    log <- onAll $ do
           log  <- getSData <|> return emptyLog
           setData emptyLog
           return log

    r <- wormhole node $ loggedc $ do
             local $ return params

             teleport
             local empty

    restoreLog log

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
   wormhole (notused 1) $ loggedc $ do
      x <- local $ return $ notused 2
      r <- onAll $ runCloud (serv x) <** setData WasRemote
      local $ return r
      teleport
      return r
   where
   notused n= error $  "runService: "++ show (n::Int) ++ " variable should not be used"
   initNodeServ servs=do
      mynode <- local $ do
        port <-  getPort
        liftIO $ createNodeServ "localhost" port servs

      listen mynode -- <|> return()
      where
      getPort :: TransIO Integer
      getPort =  if isBrowserInstance then return 0 else do
          oneThread $ option "start" "re/start node"
          input (const True) "port to listen? "

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
--localServiceMonitor ident service = keep $ runCloud  $
--    runService ("https://github.com/agocorona/transient-universe","monitor") $ do
--       initService ident service





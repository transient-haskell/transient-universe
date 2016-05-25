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
import Transient.Logged
import Transient.EVars
import Control.Monad.IO.Class
import System.Process
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Applicative
import Network (PortID(..))
import GHC.Conc
import System.Directory
import Control.Monad
import Data.List
import Data.Maybe
--import Data.TCache hiding(onNothing)

-- for the example
import System.Environment

startServices :: Cloud ()
startServices= local $ do
  node <-  getMyNode
  liftIO $ print node
  servs <-  liftIO $ readMVar $ services node
  mapM_ start  servs
  where
  start (package,program,port)= liftIO $ do
          let prog= pathExe (name package) program port
          liftIO $ print prog
          createProcess $ shell prog


pathExe package program port= package++"/dist/build/"++package++"/"++program
                                       ++ " " ++ show port

install :: String  -> String -> Int -> Cloud ()
install package program port =  do
     let packagename = name package
     exist <-  local $ liftIO $ doesDirectoryExist  packagename
     when (not exist) $ local $ liftIO $ do
         callProcess  "git" ["clone",package]
         liftIO $ print "GIT DONE"
         setCurrentDirectory packagename
         callProcess  "cabal" ["install"]
         setCurrentDirectory ".."
         return()
     let prog= pathExe packagename program port
     local $ liftIO $ do
           createProcess $ shell program
           return ()

     let service= (package, program,  port)

     Connection{myNode=Node{services=rservs}} <- onAll getSData <|> error "Mynode not set: use setMyNode"
     lliftIO $ modifyMVar_  rservs $ \servs ->  return$ service:servs
     node <-  onAll getMyNode
     notifyService node service
     return()

name url= do
     let git= "http://github.com/"
     if not $ isPrefixOf git url
       then error "install: only github repos are admitted, sorry"
       else
        let segments = split '/' $ drop (length git) url
            segs'= reverse segments
        in  head  segs'


     where
     split c []= []
     split c xs=
        let (h,t)= span  (/= c) xs
        in  if null t then [h] else h : split c  (tail t)

rfreePort :: MVar Int
rfreePort = unsafePerformIO $ newMVar  3000

freePort :: MonadIO m => m Int
freePort= liftIO $ modifyMVar rfreePort $ \ n -> return (n+1,n)

initService node package program= loggedc $ do
    services <- onAll $ liftIO $ readMVar $ services node
    case   find  (\(package', program',_) -> package==package' && program== program') $ services  of
       Just (_,_,port) -> return port
       Nothing -> do
            beamTo node
            port <- onAll freePort
            install package program  port
            empty
          <|> do
            Connection{comEvent=ev} <- onAll getSData
            (node', (package', program',port)) <- local waitNodeEvents
            if node'== node && package' == package && program'== program
                 then return port
                 else empty

notifyService :: Node -> Service -> Cloud ()
notifyService node service=  clustered $ do
     onAll . liftIO $  do
        nodes <- atomically $ readTVar nodeList

        let nod = fromMaybe (error $ "node not found :" ++  show node) $ find (== node) nodes :: Node
        modifyMVar_ (services nod) $ \servs -> return $ service:servs
        return ()

     local $ sendNodeEvent (node,service)
     return ()

{-
main= do
--      keep $ install "http://github.com/agocorona/transient" "MainStreamFiles"  3000
    let node1= createNode "localhost" 2000
    let node2= createNode "localhost" 2001
    args <-getArgs
    let [localNode,remoteNode]= if null args then [node1,node2] else [node2,node1]


    runCloudIO $ do
      onAll $ addNodes [localNode, remoteNode]
      onAll $ setMyNode localNode
      listen localNode <|> return ()
      local $ option "start" "start"

      startServices
      port <-initService remoteNode "http://github.com/agocorona/transient" "MainStreamFiles"
      onAll . liftIO $ putStrLn $ "installed at" ++ show port
-}






-----------------------------------------------------------------------------
--
-- Module      :  Transient.Move.Services.MonitorService
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Transient.Internals
import Transient.Logged
import Transient.Indeterminism(choose)
import Transient.Move.Internals
import Transient.Move.Utils
import Transient.Move.Services
import Control.Applicative
import Control.Monad.IO.Class
import Control.Exception(SomeException(..))
import Control.Concurrent
import Control.Monad
import Data.List
import System.Process
import System.Directory
import Data.Monoid
import Unsafe.Coerce
import System.IO.Unsafe
import Data.IORef
import qualified Data.Map as M
import GHC.Conc



   
main = do
   putStrLn "Starting Transient monitor"
   keep' $ runService monitorService 3000 
 
                        [serve receiveStatus
                        ,serve returnInstances
                        ,serve reReturnInstances]
                        empty 


   
pings =  do
  
  localIO $ print $ "INITIATING PINGSSSSSSSSSSSSSSSSSSSSSSS"
  local $ threads 0 $ choose ([1..] :: [Int])

  nodes <-  local getNodes 
  return () !> ("NODES=", length nodes)
            
  localIO $ threadDelay 10000000       

  local $ threads 1 $ runCloud $ mapM ping $  tail nodes
  empty

  
   
type Port= Int

receiveStatus :: (Port, String) -> Cloud ()
receiveStatus (port, logLine)= do
   localIO $ appendFile ("log"++ show port) $ logLine++"\n"
   

blockings= unsafePerformIO $ newIORef M.empty


withBlockingService :: Service -> Cloud a -> Cloud a
withBlockingService serv proc= do
   beingDone <- localIO $ atomicModifyIORef  blockings $ \map -> 
                                let mv = M.lookup serv map
                                in case mv of
                                   Nothing -> (M.insert serv () map,False)
                                   Just () -> (map,True)
   if beingDone 
    then do
         --localIO $ threadDelay 3000000
         withBlockingService serv proc
    else do
       r <- proc
       localIO $ atomicModifyIORef blockings $ \map -> (M.delete serv map,())
       return r

reReturnInstances :: (String, Node, Int) -> Cloud [Node] 
reReturnInstances (ident, node, num)=  do
      local $ delNodes [node]
      returnInstances (ident, nodeServices node, num)

returnInstances :: (String, Service, Int) -> Cloud [Node] 
returnInstances (ident, service, num)= withBlockingService service $ do

       nodes <- local $ findInNodes service >>= return . take num

       let n= num - length nodes
       if n <= 0 then return $ take num nodes 
        else  return nodes <>  requestInstall ident service n
    where

    requestInstall :: String -> Service -> Int -> Cloud [ Node]
    requestInstall ident service num=  do
      ns <- local getEqualNodes  
      return () !> ("equal",ns)    
      auth <-   callNodes' ns (<>) mempty  $  localIO $ authorizeService ident service >>=  \x -> return [x]
      return () !> auth
      let nodes = map fst $ filter  snd  $ zip ns auth 
          nnodes= length nodes
          pernode= num `div` nnodes
          lacking= num `rem` nnodes
          (nodes1,nodes2)= splitAt  lacking nodes
      return () !> (pernode,lacking,nodes1,nodes2)
      rs <- callNodes' nodes1 (<>) mempty (installHere ident service (pernode+1)) <>           
            callNodes' nodes2 (<>) mempty (installHere ident service pernode)
      local $ addNodes rs 
      ns <- onAll getNodes
      
      return rs   !>  ("MONITOR RETURN---------------------------------->", rs)
       
    -- installIt = installHere ident service <|> installThere ident service
    installHere  :: String -> Service -> Int -> Cloud [ Node]
    installHere ident service n= local $  replicateM n installOne
            where
            installOne= do
                    port <- liftIO freePort
                    install  service port
                    return () !> "INSTALLED"

                    thisNode <- getMyNode
                    let node= Node (nodeHost thisNode)  port Nothing  service   -- node to be published
                        nodelocal= Node "localhost" port Nothing [("externalNode", show $ node{nodeServices=[]})] -- local node
                    addNodes [node] --  {nodeServices=("localNode", show nodelocal{nodeServices=[]}):nodeServices node},nodelocal ]
                    return node -- {nodeServices= nodeServices node ++ [("relay",show thisNode{nodeServices=[]})]}
              `catcht` \(e :: SomeException) ->  liftIO (putStr "INSTALLLLLLLLLLLLLLL2222222: " >> print e) >> empty

      





install ::  Service  -> Int -> TransIO ()

install  service port= do
    -- return () !> "IIIIIIIIIIIIIIINSTALL"
    install'  `catcht` \(e :: SomeException) -> liftIO (putStr "INSTALL error: " >> print e) >> empty 
    where
    install'= do
        let host= "localhost"
        program <- return (lookup "executable" service) `onNothing` empty
        -- return ()  !> ("program",program)
        tryExec program host port  <|> tryDocker service host port program
                                   <|> do tryInstall service  ; tryExec program host port


tryInstall :: Service -> TransIO ()
tryInstall service = do 
    package <- emptyIfNothing (lookup "package" service) 
    install package
    where  
    install package
        | "git:" `isPrefixOf` package= installGit package  
        | "https://github.com" `isPrefixOf` package =  installGit package  
        | "http://github.com"  `isPrefixOf` package =  installGit package  


tryDocker service host port program= do
     image <- emptyIfNothing $ lookup "image" service
     path <- Transient $ liftIO $ findExecutable "docker"    -- return empty if not found
     liftIO $ callProcess path ["run", image,"-p"," start/"++host++"/"++ show port++ " " ++ program]


tryExec program host port= do
     path <-  Transient $ liftIO $ findExecutable program  -- !> ("findExecutable", program)
     spawnProgram program host port  --  !>"spawn"
     where
     spawnProgram  program host port= liftIO $ do

          let prog = pathExe  program host port
          putStr  "executing: " >> putStrLn prog
          let createprostruct= shell prog
          createProcess $ createprostruct ; return ()

          threadDelay 2000000

          -- return()                             !> ("INSTALLED", program)
          where
          
          pathExe  program host port=
                 program  ++ " -p start/" ++ show (host ::String) 
                                   ++"/" ++ show (port ::Int) ++ " > "++ program ++ host ++ show port ++ ".log  2>&1"






installGit package  = liftIO $  do

                let packagename = name package
                when (null packagename) $ error $ "source for \""++package ++ "\" not found"
                callProcess  "git" ["clone",package]
                liftIO $ putStr package >> putStrLn " cloned"
                setCurrentDirectory packagename
                callProcess  "cabal" ["install","--force-reinstalls"]
                setCurrentDirectory ".."

   
                where
                name url=  slash . slash . slash $ slash url
                  where
                  slash= tail1 . dropWhile (/='/')
                  tail1 []=[]
                  tail1 x= tail x


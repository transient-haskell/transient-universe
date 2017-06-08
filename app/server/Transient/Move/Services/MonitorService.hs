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

main = keep . runCloud $ do
    runService monitorService 3000 $ \(ident,service,num) -> do
       return () !> ("RUNSERVICE",ident, service, num)
       nodes <- local $ findInNodes service >>= return . take num
       -- onAll $ liftIO $ print  ("NODES",nodes)


       let n= num - length nodes
       if n==0 then return nodes 
        else  return nodes <>  requestInstall ident service n
    where

    requestInstall :: String -> Service -> Int -> Cloud [ Node]
    requestInstall ident service num=  do
      ns <- local getEqualNodes  
    --   return () !> ("equal",ns)    
      auth <-   callNodes' ns (<>) mempty  $  localIO $ authorizeService ident service >>=  \x -> return [x]
    --   return () !> auth
      let nodes = map fst $ filter  snd  $ zip ns auth 
          nnodes= length nodes
          pernode= num `div` nnodes
          lacking= num `rem` nnodes
          (nodes1,nodes2)= splitAt  lacking nodes
    --   return () !> (pernode,lacking,nodes1,nodes2)
      rs <- callNodes' nodes1 (<>) mempty (installHere ident service (pernode+1)) <>           
            callNodes' nodes2 (<>) mempty (installHere ident service pernode)
      local $ addNodes rs 
      return rs
       
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
                    addNodes [node{nodeServices=("localNode", show nodelocal{nodeServices=[]}):nodeServices node},nodelocal ]
                    return node {nodeServices= nodeServices node ++ [("relay",show thisNode{nodeServices=[]})]}
            -- `catcht` \(e :: SomeException) ->  liftIO (putStr "INSTALLLLLLLLLLLLLLL2222222: " >> print e) >> empty

      

-- nodeService node@(Node h _ _ _) port service=  Node h port Nothing $ service  -- ++ [("relay",show $node{nodeServices=[]})



install ::  Service  -> Int -> TransIO ()

install  service port= do
    return () !> "IIIIIIIIIIIIIIINSTALL"
    install'   `catcht` \(e :: SomeException) -> liftIO (putStr "INSTALL error: " >> print e) >> empty 
    where
    install'= do
        let host= "localhost"
        program <- return (lookup "executable" service) `onNothing` empty
        return ()  !> ("program",program)
        tryExec program host port  <|> tryDocker service host port 
                                   <|> do tryInstall service  ; tryExec program host port

emptyIfNothing :: Maybe a -> TransIO a
emptyIfNothing =  Transient  . return

tryInstall :: Service -> TransIO ()
tryInstall service = do 
    package <- emptyIfNothing (lookup "package" service) 
    install package
    where  
    install package
        | "git:" `isPrefixOf` package= installGit package  
        | "https://github.com" `isPrefixOf` package =  installGit package  
        | "http://github.com" `isPrefixOf` package =  installGit package  


tryDocker service host port= do
     image <- emptyIfNothing $ lookup "image" service
     path <- Transient $ liftIO $ findExecutable "docker"    -- return empty if not found
     liftIO $ callProcess path ["run", image,"-p","start/"++host++"/"++ show port]


tryExec program host port= do
     path <-  Transient $ liftIO $ findExecutable program   !> ("findExecutable", program)
     spawnProgram program host port    !>"spawn"
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
                                   ++"/" ++ show (port ::Int) ++ " > "++ program ++ host ++ show port ++ ".log"






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


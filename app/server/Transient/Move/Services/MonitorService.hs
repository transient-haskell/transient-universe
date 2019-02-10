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
import System.IO
import System.Process
import System.Directory
import Data.Monoid
import Unsafe.Coerce
import System.IO.Unsafe
import Data.IORef
import qualified Data.Map as M
import GHC.Conc
import Data.Maybe(fromMaybe)
import Control.Exception
import qualified Data.ByteString.Lazy.Char8   as BS
import qualified Data.ByteString.Char8 as BSS



   
main = do
   putStrLn "Starting Transient monitor"
   keep $ runService monitorService 3000 
 
                        [serve receiveStatus
                        ,serve returnInstances
                        ,serve reReturnInstances
                        
                        ,serve receiveFromNodeStandardOutputIt
                        ,serve sendToNodeStandardInputIt
                        ,serve getLogIt
                        ]
                        empty 


{- ping is not used to determine healt of services. The client program notify the
   monitor when a service fails, with reInitService.
pings =  do
  
  localIO $ print $ "INITIATING PINGSSSSSSSSSSSSSSSSSSSSSSS"
  local $ threads 0 $ choose ([1..] :: [Int])

  nodes <-  local getNodes 
  return () !> ("NODES=", length nodes)
            
  localIO $ threadDelay 10000000       

  local $ threads 1 $ runCloud $ mapM ping $  tail nodes
  empty
-}
  
   
type Port= Int

-- | receive a status from an executable.
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

-- | gets a node with a service, which probably failed and return other n instances of the same service.
-- This is used to implement failover.
reReturnInstances :: (String, Node, Int) -> Cloud [Node] 
reReturnInstances (ident, node, num)=  do
      local $ delNodes [node]
      returnInstances (ident, nodeServices node, num)

-- | install and return n instances of a service, distributed
-- among all the nodes which have monitoService executables running and connected 
returnInstances :: (String, Service, Int) -> Cloud [Node] 
returnInstances (ident, service, num)= withBlockingService service $ do
       return () !> "RETURNINSTANCES"
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
      rs <- callNodes' nodes1 (<>) mempty (installHere  service (pernode+1)) <>           
            callNodes' nodes2 (<>) mempty (installHere  service pernode)
      local $ addNodes rs 
      ns <- onAll getNodes
      
      return rs   !>  ("MONITOR RETURN---------------------------------->", rs)
       
    -- installIt = installHere  service <|> installThere  service
    installHere  ::  Service -> Int -> Cloud [ Node]
    installHere  service n= local $  replicateM n installOne
            where
            installOne= do
                    port <- liftIO freePort
                    install  service port
                    return () !> "INSTALLED"

                    thisNode <- getMyNode
                    let node= Node (nodeHost thisNode)  port Nothing  (service ++ relayinfo thisNode)  -- node to be published
                    addNodes [node] 
                    return node
              `catcht` \(e :: SomeException) ->  liftIO (putStr "INSTALLLLLLLLLLLLLLL2222222: " >> print e) >> empty
              
            relayinfo mon= if nodeHost mon /= "localhost" then [("relay",show(nodeHost mon,nodePort mon))] else []
      





install ::  Service  -> Int -> TransIO ()

install  service port= do
    -- return () !> "IIIIIIIIIIIIIIINSTALL"

    install'  `catcht` \(e :: SomeException) -> liftIO (putStr "INSTALL error: " >> print e) >> empty 
    where
    install'= do
        my <- getMyNode
        let host= nodeHost my
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
     liftIO $ callProcess path ["run", image,"-p"," start/"++ host++"/"++ show port++ " " ++ program]


tryExec program host port= do
     path <-  Transient $ liftIO $ findExecutable program  -- would abandon (empty) if the executable is not found
     spawnProgram program host port  --  !>"spawn"
     where
     spawnProgram  program host port= do

          let prog = pathExe  program host port
          liftIO $ putStr  "executing: " >> putStrLn prog

          (networkExecuteStreamIt prog >> empty) <|> return () !> "INSTALLING"
          liftIO $ threadDelay 2000000

          return()                             !> ("INSTALLED", program)
          where
          
pathExe  program host port=
                 program  ++ " -p start/" ++  (host ::String) 
                                   ++"/" ++ show (port ::Int) -- ++ " > "++ program ++ host ++ show port  ++ ".log  2>&1"






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


-------------------------execution ----------------------------

getLogIt :: GetLog -> Cloud BS.ByteString
getLogIt (GetLog node)= do
    let program = fromMaybe (error "no Executable in service "++ show (nodeServices node)) $
                             lookup "executable"  (nodeServices node)
    let expr = pathExe program (nodeHost node) (nodePort node)
    localIO $ BS.readFile $ logFileName expr


sendToNodeStandardInputIt :: (Node, String) -> Cloud ()
sendToNodeStandardInputIt (node,inp)= do
    let program = fromMaybe (error "no Executable in service "++ show (nodeServices node)) $
                             lookup "executable"  (nodeServices node)
        expr= pathExe program (nodeHost node) (nodePort node)
    return () !> ("SEND TO NODE STANDARD INPUT", program, expr)
    sendExecuteStreamIt1 (expr, inp)
    where
    sendExecuteStreamIt1 (cmdline, inp)= localIO $ do
       map <- readIORef rinput 
       let input1= fromMaybe (error "this command line has not been opened") $ M.lookup cmdline map 
       hPutStrLn input1 inp 
       hFlush input1
       return()
       
receiveFromNodeStandardOutputIt :: ReceiveFromNodeStandardOutput -> Cloud String
receiveFromNodeStandardOutputIt (ReceiveFromNodeStandardOutput node ident) = local $ do
    let program = fromMaybe (error "no Executable in service "++ show (nodeServices node)) $
                             lookup "executable"  (nodeServices node)
        expr= pathExe program (nodeHost node) (nodePort node)
    return () !> ("RECEIVE FROM STANDARD OUTPUT",expr)
    labelState ident
    getMailbox' ("output"++ expr)

rinput :: IORef (M.Map String Handle)
rinput= unsafePerformIO $ newIORef M.empty 


logFolder= "./.log/"

logFileName ('.':expr) = logFileName expr
logFileName expr= logFolder ++ subst expr ++ ".log"
    where
    subst []= [] 
    subst (' ':xs)= '-':subst xs
    subst ('/':xs)= '-':subst xs
    subst ('\"':xs)= '-':subst xs
    subst (x:xs)= x:subst xs   

-- | execute the shell command specified in a string and stream back at runtime -line by line- the standard output
-- as soon as there is any output. It also stream all the standard error in case of exiting with a error status.
-- to the service caller. invoked by `networkExecuteStream`.

      
networkExecuteStreamIt :: String  -> TransIO String
networkExecuteStreamIt expr  =  do
      liftIO $ createDirectoryIfMissing True logFolder
      
      r <- liftIO $ createProcess $ (shell expr){std_in=CreatePipe,std_err=CreatePipe,std_out=CreatePipe}
      liftIO $ atomicModifyIORef rinput $ \map ->   (M.insert expr (input1 r) map,())
   
      let logfile= logFileName  expr 
      
      hlog <- liftIO $ openFile logfile WriteMode 
      liftIO $ hPutStrLn  hlog expr
      liftIO $ hClose hlog    
      
      line <- watch (output r) <|> watch (err r) <|> watchExitError r 
      putMailbox' ("output" ++ expr) line
      hlog <- liftIO $ openFile logfile AppendMode 
      liftIO $ hPutStrLn  hlog line
      liftIO $ hClose hlog    
      return line
      where

      input1 r= inp where (Just inp,_,_,_)= r
      output r= out where (_,Just out,_,_)= r
      err r= err where    (_,_,Just err,_)= r
      handle r= h where   (_,_,_,h)= r

      watch :: Handle -> TransIO String
      watch h=    do
        abduce
        mline  <-  threads 0 $ (parallel $  (SMore <$> hGetLine' h) `catch` \(e :: SomeException) -> return SDone)
        case mline of
           SDone -> empty
           SMore line ->  return line
           
        where

        hGetLine' h= do
          buff <- newIORef []
          getMore buff
          
          where

          getMore buff= do
            b <- hWaitForInput h 10
            if not b
                then do
                   r <-readIORef buff
                   if null r then getMore buff else return r
                else do
                      c <- hGetChar h
                      if c== '\n' then readIORef buff else do
                        modifyIORef buff $ \str -> str ++ [c]
                        getMore buff

      watchExitError r= do    -- make it similar to watch
        abduce
        liftIO $ waitForProcess $ handle r
        errors <- liftIO $  hGetContents (err r)
        return errors


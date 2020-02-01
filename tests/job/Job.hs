#!/usr/bin/env execthirdlinedocker.sh
--  info: use sed -i 's/\r//g' file if report "/usr/bin/env: ‘execthirdlinedocker.sh\r’: No such file or directory"
-- LIB="/home/vsonline/workspace" && ghc  -DDEBUG -threaded  -i${LIB}/transient/src -i${LIB}/transient-universe/src -i${LIB}/transient-universe-tls/src -i${LIB}/axiom/src   $1 && ./`basename $1 .hs` ${2} ${3}

{-# LANGUAGE ScopedTypeVariables #-}
import Transient.Internals 
import Transient.Logged hiding(checkpoint, restore, restore')
import Transient.Indeterminism
import Transient.Parse
import Transient.Move.Internals
import Transient.Move.Utils
import Transient.Internals
import Control.Exception hiding (onException)
import Data.Typeable
import System.Directory
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import System.IO.Unsafe
import Control.Concurrent.MVar
import Debug.Trace 
import qualified Data.ByteString.Char8 as BS
import System.Random
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Monoid 
import Control.Concurrent
import Control.Monad.State
data CurrentDirectory= CurrentDirectory String

restartable :: Loggable a => String -> Cloud a  -> Cloud(Either String a)
restartable  jobname job= do
    local abduce
    local $ do
        r <- liftIO $ doesDirectoryExist jobname
        --when  r $ removeDirectoryRecursive jobname
        when (not r)   $ liftIO$ createDirectory  jobname 
        cur <- liftIO $ getCurrentDirectory 
        setData $ CurrentDirectory $ cur ++ '/':jobname  --quitarÑ
    restorable job
    where
    restorable job= restore' $ do
     r <- (Right <$> job) `catchc` \(e :: SomeException) -> return . Left $ show e
     case r of
       Left err -> do
            localIO $ print "ERRRRRR"
            node <- local getMyNode
            (r,n) <- atConsole $ \n  -> do 
              --let n= 0 :: Int
              --let shown= show n
              liftIO $ do putStrLn $ "---- job "++ jobname ++ " error ----\n"
                          putStrLn err
                          putStrLn "\nat node: ";
                          print node
                          putStrLn "\n---- end error ----\n"

              let showerr=  jobname ++ "-showerror"
                  cont=     jobname ++ "-cont"  
                  restart=  jobname ++ "-restart"
                  cancel=   jobname ++ "-cancel"
                   
              r <-  ( do option showerr "show the error message" ; liftIO $ print (err,node) ; empty) <|>
                    option cont "continue execution if you had solved the problem" <|>
                    option restart "restart" <|>
                    option  cancel "cancel the job"
              
              liftIO $ mapM delConsoleAction [showerr,cont, restart, cancel]
              
              return $ (drop (length jobname +1 ) r,n) 
            localIO $ print r
          
            case r of
              "cont"    ->  restorable  job
              "restart" ->  do delLog; restorable job
              "cancel"  ->  localIO $ do 
                   print  $ "job " ++ show n ++" cancelled"
                   return $ Left err 
              
       Right r -> return $ Right r
   
catchc :: (Loggable a,Exception e) => Cloud a -> (e -> Cloud a) -> Cloud a
catchc (Cloud c) ex= Cloud $ logged $ c `catcht` \e ->  runCloud' $ ex e 



logsname= "logs/"
newtype RestoreInit= RestoreInit Int


restore' :: Cloud a -> Cloud a
restore' proc = Cloud $ do
     setRestoreInit
     modifyData' (\log -> log{buildLog=mempty}) emptyLog{buildLog=mempty}
     CurrentDirectory curDir <- getSData <|> return (CurrentDirectory ".")
     let logs=  curDir++ '/':logsname
     liftIO $ createDirectory logs  `catch` (\(e :: SomeException) -> return ())
     list <- liftIO $ getDirectoryContents logs
                 `catch` (\(e::SomeException) -> return [])
     if null list || length list== 2 then runCloud' proc else do

         let list'= filter ((/=) '.' . head) list
         file <- choose  list'

         log <- liftIO $ BSL.readFile (logs++file)
         return () !!>  ("LOG",log)
         --let logb= lazyByteString  log
         -- setData Log{recover= True,buildLog= logb,fulLog= logb,lengthFull= 0, hashClosure= 0}
         modifyData' (\log -> log{recover=True}) emptyLog{recover=True}
         
         setParseString log
         liftIO $ remove $ logs ++ file


         runCloud' proc

     where
     remove f=  removeFile f `catch` (\(_::SomeException) -> remove f)
     setRestoreInit= do 
        log <- getLog
        setState $ RestoreInit $ fromIntegral $ BSL.length $ toLazyByteString $ fulLog log


-- | Saves the accumulated logs of the current computation, like 'suspend', but
-- does not exit.
checkpoint :: TransIO ()
checkpoint = do
   log <- getLog
   if (recover log) then return () else logAll  $ fulLog log

delLog = local $ do
    CurrentDirectory curDir <- getSData <|> return (CurrentDirectory ".")
    liftIO $ removeDirectoryRecursive curDir

logAll log= do
    CurrentDirectory curDir <- getSData <|> return (CurrentDirectory ".")
    let logs=  curDir++ '/':logsname
    newlogfile <- liftIO $ (logs ++) <$> replicateM 7 (randomRIO ('a','z'))
    logsExist <- liftIO $ doesDirectoryExist logs
    when (not logsExist) $ liftIO $ createDirectory logs
    RestoreInit len <- getState <|> return (RestoreInit 0)
    liftIO $ BSL.writeFile newlogfile $  BSL.drop (fromIntegral len) $ toLazyByteString  log
        -- :: TransIO ()



data Console = Console Node Int deriving (Typeable, Read, Show)
instance Loggable Console

rconsole= unsafePerformIO $ newEmptyMVar

atConsole :: Loggable a => (Int -> TransIO a) -> Cloud a
atConsole todo= do
  mconsole@(Console node n) <- localIO $ readMVar rconsole
  runAt node $ local $ todo n -- do local $ setRState (Console $ n +1); local $ todoxx  return r

(!!>) x y = (flip trace) x (show y)   


main1 = keep $ runCloud'  proc
  where
  proc= restore' $ do
      localIO $ print "main"
      r <- (Right <$> job) `catchc` \(e :: SomeException) -> return . Left $ show e
      case r of
        Left err -> do
            localIO $ print err
            proc
      return()
  
  job= do
     localIO $ print "RUNNING"
     error "error1"
     return ()


main3=  keep $  runCloud' $ do
 local $ return "hello"

 restore' $  do 
    local $ liftIO $ print "hello"
    local $ do
        log <- getLog 
        if recover log then return () else checkpoint >> exit ()
    local $ liftIO $ print "world"

main5= keep  . initNode $ inputNodes <|> process
    where
   
    process= do
        local $ option "go"  "execute"
        nodes <- local getNodes

        guard $ length nodes > 1
        mynode <-  local getMyNode
        let othernode = nodes !! 1
        mclustered $ localIO $ putMVar rconsole $ Console mynode 0
        modify $ \s-> s{execMode=Parallel}

        local $ onException $ \(e:: SomeException) -> liftIO $ print e
        -- forkc showlog
        
        r <- (,) <$> pr "job1" othernode <*> pr "job2" othernode 
        localIO $ print r
        where
        {-
        showlog= do
          (node,jobname) <- clustered $ local $ getMailbox' "jobs" :: Cloud (Node,String)
          local $ option msg msg
          
          r <- wormhole node $ do
                  stopRemoteJob  $ BS.pack "streamlog"
                  atRemote $ local $ getMailbox' jobname !> ("getmailbox", jobname)
          localIO $ putStrLn r
        -}    
        pr jobname node = runAt node $ restartable jobname  $   do

               localIO $ print $"EXECUTING " ++ jobname
               onAll $ checkpoint
               onAll $ do 
                log <- getLog
                if recover log then return () else error "break"
               -- error "break" -- if (n `rem` (10 :: Int)) ==0 then error "break" else setRState (n +1) 
               localIO $ print $ "continuing "++ jobname
               return $ "ended "++ jobname

main= keep $ initNode $ inputNodes <|> do 
     local $ option "g"  "go"
     node1 <- local $ getNodes >>= return . (!! 1)
     runAt node1 $ localIO $ return ()
     --mode <- gets execMode 
     --localIO $ print mode
     --modify $ \s -> s{execMode= Serial}
     r <- runAt node1 delay <> runAt node1 delay
     localIO $ print r
     where
     delay= local $  do
         liftIO $ threadDelay 4000000
         option "r" "respond"
        
   {- 
    process1=   do
         requires ["ls","docker"]
         let folder= "dropfolder"
         local $ do
            file <- changed ("ls  static/out.jsexe/" ++ folder) 3 
            return () !> "FILE"
            url <- generateURL $ folder ++ "/"++ file
            return () !> ("URL", url)
            newVar "url" url
         localIO $ print "NEW FILE"
         result <- invoke0 "image typelead/eta\ninvoke cd $url && etlas install directory-1.3.1.0 && etlas install && echo success"   
                   <> invoke0 "image agocorona/transient:01-27-2017\ninvoke cd $url && cabal update && cabal install && echo success"    -- use all the nodes
         
         localIO $ putStrLn result
-}
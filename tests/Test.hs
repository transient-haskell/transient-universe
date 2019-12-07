#!/usr/bin/env execthirdlinedocker.sh
--  info: use sed -i 's/\r//g' file if report "/usr/bin/env: ‘execthirdlinedocker.sh\r’: No such file or directory"
-- runghc  -DDEBUG  -i../transient/src -i../transient-universe/src -i../axiom/src   $1 ${2} ${3}

-- mkdir -p ./static && ghcjs --make   -i../transient/src -i../transient-universe/src  -i../axiom/src   $1 -o static/out && runghc   -i../transient/src -i../transient-universe/src -i../axiom/src   $1 ${2} ${3}


-- cd /projects/transient && cabal install -f debug --force-reinstalls && cd ../transient-universe && cabal install --force-reinstalls &&  runghc $1 $2 $3 $4


                     

{-# LANGUAGE ScopedTypeVariables, CPP, FlexibleInstances, FlexibleContexts, UndecidableInstances, RecordWildCards, MultiParamTypeClasses #-}


import Transient.Internals

import Transient.Move.Internals
import Transient.Move.Utils
import Transient.Logged
import Transient.Parse
import Control.Monad.State
import System.IO (hFlush,stdout) 
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Applicative
import System.Time
import Control.Concurrent(threadDelay)
import Control.Exception hiding(onException)
import Data.IORef
import Control.Monad(when) 
import Data.Typeable
import System.Random
import Data.Maybe 
import qualified Data.Map as M
import System.Environment
import Data.List(isPrefixOf)
import Unsafe.Coerce
import Data.Monoid 
import Transient.Indeterminism
import Transient.Move.Services
import Control.Exception hiding(onException)
import System.IO
import Control.Concurrent.MVar
#ifndef ghcjs_HOST_OS
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BSS
import Data.ByteString.Builder
import Data.String
import GHC.IO.Handle
#endif

--instance Loggable BS.ByteString where
--  serialize x= byteString $ toStrict x
--  deserialize= tTakeWhile (/='/')


#define shouldRun(x)    (local $ do p <-getMyNode; liftIO $ print (p,x) ;assert ( p == (x)) (liftIO $ print p))

main= keep $  initNode $ inputNodes <|> do
    local $ option "r" "r"
    nodes <- local getNodes
    let node0= nodes !! 0
    let node1= nodes !! 1
    let node2= nodes !! 2
    runAt node1 $ runAt node0  $ do localIO $  print "HELLO" -- ; empty :: Cloud ()
    localIO $ print "WORLD"
 
     
{-
    empty
    runAt (node1) $ do
       shouldRun (node1)
       localIO $ print "hello"  
       runAt node2 $  (runAt node1 $ do shouldRun(node1) ; localIO $ print "HELLO" ) -- <|>  (shouldRun(node2) >> return "world")
-} 
main8= keep $  initNode $ do
     r <-  return ("hi ") <>  do localIO $ print "WORLD"; local $ async (  return "hello ")   <> return "world"
     localIO $ print r

servname= [("name","serv")]

main9= keep $ runService servname 3000  [serve plus2 ] $ trans  <|> service  -- <|> callNodes1
  -- return ()

plus2 (x :: Int) = do
   return () !> ("PLUS2",x)
   return $ x + 2

callNodes1= do
    local $ option "call" "callNodes"
    node <- local getMyNode
    nodes <- local $ findInNodes servname
    r <- callNodes' nodes (<>) mempty $ return "HELLO" 
    localIO $ print r

trans = do

    return () !> "BEFORE OPTION"
    local $ option (str "t") "trans" 
    callNode <- local getMyNode
    nodes <- local getNodes

    n <- local $ return (1 :: Int)  
    r <- runAt (nodes !! 1)  $ localIO $ do print callNode ;return $ n +1
    localIO $ print r 
    trans


service = do
    local $ option (str "s") "service"
    nodes <- local getNodes
    n <- callService' (nodes !! 1) (1 :: Int)
    
    onAll $ do
        log <- getLog
        rest <- giveParseString
        return () !> ("LOG AFTER service 111",rest,recover log, buildLog log, fulLog log)    
    localIO $ return "PEPE"
    onAll $ do
        log <- getLog
        rest <- giveParseString
        return () !> ("LOG AFTER service 222",rest,recover log, buildLog log, fulLog log)    
    localIO $ print (n :: Int)
    localIO $ print (n :: Int)
    onAll $ do
        log <- getLog
        rest <- giveParseString
        return () !> ("LOG AFTER service 333",rest,recover log, buildLog log, fulLog log)   
  
    local $ option (str "tt") "transs" 
 




    r <- runAt (nodes !! 1)  $ localIO $ return $ n +1
    localIO $ print r 


{-
newtype Serv= Serv BS.ByteString deriving (Read,Show,Typeable)

instance Loggable Serv where
   serialize (Serv s)= byteString (BSS.pack "srv") <> lazyByteString s
   deserialize= symbol (BS.pack "srv") >> (Serv <$> tTakeWhile (/= '/'))


nserve :: (Loggable a, Loggable b) => BS.ByteString -> (a -> Cloud b) -> Cloud ()
nserve name serv= do
    Serv n <- local empty -- aqui puede registrar el sub-servicio.
    if n /= name then empty else do
        p <- local empty
        loggedc $ serv p
        teleport
-}

   

main6 = keep $ initNode $ inputNodes <|> do
     local $ option (str "go") "go"
     nodes <- local getNodes
     localFix
     local $ option (str "send") "send"
     world <- runAt (nodes !! 1) $  localIO $ print "HELLO"  >> return "WORld"
     localIO $ print world

     localFixServ False True
     local $ option (str "send2") "send2"
     world2 <- runAt (nodes !! 1) $  localIO $ print "HELLO2"  >> return "WORld2"
     localIO $ print world2
     
main3 =  keep $ initNode $ inputNodes <|>  do
     local $ option (str "go")  "go"
     nodes <- local  getNodes
     
     r1 <- loggedc' $ wormhole (nodes !! 1) $  do
                 teleport 
                 
                 r <- localIO $ print "HELLO" >> return "WORLD"
                 teleport 
                 localIO $ print "WORLD" >> return r

     r2 <- wormhole  (nodes !! 1) $ loggedc $ do

                     teleport 
                     r <- local $ getSData <|>  return "NOOOOOO DAAATAAAAAA"
                     localIO $ print r
                     r <- loggedc $ do localIO $ print $ "HELLO22222" ++ r1;return $ "hello2" ++ r1      
                     teleport 
                     
                     return r
     localIO $ print $ "WORLD2222222" ++  r2    

     
main5  = keep $ initNode $ inputNodes <|>  do
     local $ option (str "go")  "go"
     nodes <- local getNodes
     t1 <- onAll $ liftIO $ getClockTime
 --    r <- runAt (nodes !! 1) $ return "HELLOOOOOOO"
     wormhole (nodes !! 1) $ do
       r  <- atRemote $ localIO $ BS.pack <$> replicateM 10 (randomRIO('a','z'))
       r2 <- atRemote $ localIO $ BS.pack <$> replicateM 10 (randomRIO('a','z'))

       localIO $ print $  "RETURNED: " <>  show (r,r2)
       t2 <- onAll $ liftIO $ getClockTime 
       localIO $ print  $ diffClockTimes  t2 t1
     
main2 = keep $ initNode $ inputNodes <|>  do
     local $ option (str "go")  "go"
     nodes <- local getNodes
     runAt (nodes !! 1) $ local $ do
           n <-  getMyNode
           handle <- liftIO $ openFile ("test"++ show (nodePort n)) AppendMode
           setData handle

     append  nodes  <|> close  nodes 
     
append  nodes  = do  
     local $ option (str "wr")  "write"

     runAt (nodes !! 1) $ local $ do
         handle <- getSData <|> error "no handle"
         liftIO $ hPutStrLn handle "hello"

str s= s :: String

close nodes  =  do
        local $ option  (str "cl") "close"

        runAt (nodes !! 1) $ local $ do
             handle <- getSData <|> error "no handle"
             liftIO $ hClose handle

     
     
     
     

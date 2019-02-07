-----------------------------------------------------------------------------
--
-- Module      :  Transient.Move.Internals
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



{-# LANGUAGE DeriveDataTypeable , ExistentialQuantification, OverloadedStrings,FlexibleInstances, UndecidableInstances
    ,ScopedTypeVariables, StandaloneDeriving, RecordWildCards, FlexibleContexts, CPP
    ,GeneralizedNewtypeDeriving #-}
module Transient.Move.Internals where

import Transient.Internals 
import Transient.Parse
import Transient.Logged 
import Transient.Indeterminism

import Transient.EVars


import Data.Typeable
import Control.Applicative
import System.IO.Error
import System.Random
import           Data.String
import qualified Data.ByteString.Char8                  as BC

#ifndef ghcjs_HOST_OS
import Network
--- import Network.Info
import Network.URI
--import qualified Data.IP                              as IP
import qualified Network.Socket                         as NS
import qualified Network.BSD                            as BSD
import qualified Network.WebSockets                     as NWS -- S(RequestHead(..))

import qualified Network.WebSockets.Connection          as WS

import           Network.WebSockets.Stream hiding(parse)

import qualified Data.ByteString                        as B(ByteString,concat)

import qualified Data.ByteString.Lazy.Internal          as BLC
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.ByteString.Lazy.Char8             as BS
import           Network.Socket.ByteString              as SBS(sendMany,sendAll,recv)
import qualified Network.Socket.ByteString.Lazy         as SBSL
import           Data.CaseInsensitive(mk,CI)
import           Data.Char
import           Data.Aeson

-- import System.Random

#else
import           JavaScript.Web.WebSocket
import qualified JavaScript.Web.MessageEvent           as JM
import           GHCJS.Prim (JSVal)
import           GHCJS.Marshal(fromJSValUnchecked)
import qualified Data.JSString                          as JS


import           JavaScript.Web.MessageEvent.Internal
import           GHCJS.Foreign.Callback.Internal (Callback(..))
import qualified GHCJS.Foreign.Callback                 as CB
import           Data.JSString  (JSString(..), pack)

#endif


import Control.Monad.State
import Control.Exception hiding (onException,try)
import Data.Maybe
--import Data.Hashable


import System.IO.Unsafe
import Control.Concurrent.STM as STM
import Control.Concurrent.MVar

import Data.Monoid
import qualified Data.Map as M
import Data.List (nub,(\\),intersperse) -- ,find, insert)
import Data.IORef

import Control.Concurrent

import System.Mem.StableName
import Unsafe.Coerce



{- TODO
  timeout for closures: little smaller in sender than in receiver
-}

--import System.Random

#ifdef ghcjs_HOST_OS
type HostName  = String
newtype PortID = PortNumber Int deriving (Read, Show, Eq, Typeable)
#endif

data Node= Node{ nodeHost   :: HostName
               , nodePort   :: Int
               , connection :: Maybe (MVar Pool)
               , nodeServices   :: Service
               }

         deriving (Typeable)

instance Ord Node where
   compare node1 node2= compare (nodeHost node1,nodePort node1)(nodeHost node2,nodePort node2)


-- The cloud monad is a thin layer over Transient in order to make sure that the type system
-- forces the logging of intermediate results
newtype Cloud a= Cloud {runCloud' ::TransIO a} deriving (AdditionalOperators,Functor,
#if MIN_VERSION_base(4,11,0) 
                   Semigroup,
#endif
                   Monoid,Applicative,Alternative, Monad, Num, Fractional, MonadState EventF)



-- | Execute a distributed computation inside a TransIO computation.
-- All the  computations in the TransIO monad that enclose the cloud computation must be `logged`
runCloud :: Cloud a -> TransIO a

runCloud x= do
       closRemote  <- getSData <|> return (Closure  0)
       runCloud' x <*** setData  closRemote


--instance Monoid a => Monoid (Cloud a) where
--  f mappend x y = mappend <$> x <*> y
--   mempty= return mempty

#ifndef ghcjs_HOST_OS

--- empty Hooks for TLS

{-# NOINLINE tlsHooks #-}
tlsHooks ::IORef (SData -> BS.ByteString -> IO ()
                 ,SData -> IO B.ByteString
                 ,NS.Socket -> BS.ByteString -> TransIO ()
                 ,String -> NS.Socket -> BS.ByteString -> TransIO ())
tlsHooks= unsafePerformIO $ newIORef
                 ( notneeded
                 , notneeded
                 , \_ i -> tlsNotSupported i
                 , \_ _ _-> return())

  where
  notneeded= error "TLS hook function called"



  tlsNotSupported input = do
     if ((not $ BL.null input) && BL.head input  == 0x16)
       then  do
         conn <- getSData
         sendRaw conn $ BS.pack $ "HTTP/1.0 525 SSL Handshake Failed\r\nContent-Length: 0\nConnection: close\r\n\r\n"
       else return ()

(sendTLSData,recvTLSData,maybeTLSServerHandshake,maybeClientTLSHandshake)= unsafePerformIO $ readIORef tlsHooks


#endif

-- | Means that this computation will be executed in the current node. the result will be logged
-- so the closure will be recovered if the computation is translated to other node by means of
-- primitives like `beamTo`, `forkTo`, `runAt`, `teleport`, `clustered`, `mclustered` etc
local :: Loggable a => TransIO a -> Cloud a
local =  Cloud . logged

--stream :: Loggable a => TransIO a -> Cloud (StreamVar a)
--stream= Cloud . transport

-- #ifndef ghcjs_HOST_OS
-- | Run a distributed computation inside the IO monad. Enables asynchronous
-- console input (see 'keep').
runCloudIO :: Typeable a =>  Cloud a -> IO (Maybe a)
runCloudIO (Cloud mx)= keep mx

-- | Run a distributed computation inside the IO monad with no console input.
runCloudIO' :: Typeable a =>  Cloud a -> IO (Maybe a)
runCloudIO' (Cloud mx)=  keep' mx

-- #endif

-- | alternative to `local` It means that if the computation is translated to other node
-- this will be executed again if this has not been executed inside a `local` computation.
--
-- > onAll foo
-- > local foo'
-- > local $ do
-- >       bar
-- >       runCloud $ do
-- >               onAll baz
-- >               runAt node ....
-- > callTo node' .....
--
-- foo bar and baz will e executed locally.
-- Here foo will be executed remotely also in node' but foo' bar and baz don't.
--

--

onAll ::  TransIO a -> Cloud a
onAll =  Cloud

-- | only executes if the result is demanded. It is useful when the conputation result is only used in
-- the remote node, but it is not serializable. All the state changes executed in the argument with 
-- `setData` `setState` etc. are lost
lazy :: TransIO a -> Cloud a
lazy mx= onAll $ getCont >>= \st -> Transient $
        return $ unsafePerformIO $ runStateT (runTrans mx) st >>=  return .fst 


-- | executes a non-serilizable action in the remote node, whose result can be used by subsequent remote invocations
fixRemote mx= do
             r <- lazy mx
             fixClosure
             return r

-- | experimental: subsequent remote invocatioms will send logs to this closure. Therefore logs will be shorter. 
--
-- Also, non serializable statements before it will not be re-executed
fixClosure= atRemote $ local $  async $ return ()

-- log the result a cloud computation. like `loogged`, this erases all the log produced by computations
-- inside and substitute it for that single result when the computation is completed.
loggedc :: Loggable a => Cloud a -> Cloud a
loggedc (Cloud mx)= Cloud $ do
     closRemote  <- getSData <|> return (Closure  0 )
     (fixRemote :: Maybe (Int,Int,IORef (M.Map Int Int))) <- getData 
     logged mx <*** do setData  closRemote 
                       when (isJust fixRemote) $ setState (fromJust fixRemote)


loggedc' :: Loggable a => Cloud a -> Cloud a
loggedc' (Cloud mx)= Cloud $ logged mx
  

    

-- | the `Cloud` monad has no `MonadIO` instance. `lliftIO= local . liftIO`
lliftIO :: Loggable a => IO a -> Cloud a
lliftIO= local . liftIO

-- |  `localIO = lliftIO`
localIO :: Loggable a => IO a -> Cloud a
localIO= lliftIO



-- | continue the execution in a new node
beamTo :: Node -> Cloud ()
beamTo node =  wormhole node teleport


-- | execute in the remote node a process with the same execution state
forkTo  :: Node -> Cloud ()
forkTo node= beamTo node <|> return()

-- | open a wormhole to another node and executes an action on it.
-- currently by default it keep open the connection to receive additional requests
-- and responses (streaming)
callTo :: Loggable a => Node -> Cloud a -> Cloud a
callTo node  remoteProc= wormhole node $ atRemote remoteProc
 

#ifndef ghcjs_HOST_OS
-- | A connectionless version of callTo for long running remote calls
callTo' :: (Show a, Read a,Typeable a) => Node -> Cloud a -> Cloud a
callTo' node remoteProc=  do
    mynode <-  local $ getNodes >>= return . head
    beamTo node
    r <-  remoteProc
    beamTo mynode
    return r
#endif

-- | Within a connection to a node opened by `wormhole`, it run the computation in the remote node and return
-- the result back to the original node.
--
-- If `atRemote` is executed in the remote node, then the computation is executed in the original node
--
-- > wormhole node2 $ do
-- >     t <- atRemote $ do
-- >           r <- foo              -- executed in node2
-- >           s <- atRemote bar r   -- executed in the original node
-- >           baz s                 -- in node2
-- >     bat t                      -- in the original node

atRemote :: Loggable a => Cloud a -> Cloud a
atRemote proc= loggedc' $ do
     teleport                                              -- !> "teleport 1111"
     modifyData' f1 NoRemote
     r <-  loggedc $ proc  <** setData WasRemote
     teleport                                              -- !> "teleport 2222"

     return r
     where
     f1 WasParallel= WasParallel
     f1 _= NoRemote

-- | Execute a computation in the node that initiated the connection. 
--
-- if the sequence of connections is  n1 -> n2 -> n3 then  `atCallingNode $ atCallingNode foo` in n3 
-- would execute `foo` in n1, -- while `atRemote $ atRemote foo` would execute it in n3
-- atCallingNode :: Loggable a => Cloud a -> Cloud a
-- atCallingNode proc=  connectCaller $ atRemote proc 

-- | synonymous of `callTo`
runAt :: Loggable a => Node -> Cloud a -> Cloud a
runAt= callTo



-- | run a single thread with that action for each connection created.
-- When the same action is re-executed within that connection, all the threads generated by the previous execution
-- are killed
--
-- >   box <-  foo
-- >   r <- runAt node . local . single $ getMailbox box
-- >   localIO $ print r
--
-- if foo  return different mainbox indentifiers, the above code would print the
-- messages of  the last one.
-- Without single, it would print the messages of all of them since each call would install a new `getMailBox` for each one of them
single :: TransIO a -> TransIO a
single f= do
   cutExceptions
   Connection{closChildren=rmap} <- getSData <|> error "single: only works within a connection"
   mapth <- liftIO $ readIORef rmap
   id <- liftIO $ f `seq` makeStableName f >>= return .  hashStableName


   case  M.lookup id mapth of
          Just tv -> liftIO $ killBranch'  tv
          Nothing ->  return ()


   tv <- get
   f <** do
          id <- liftIO $ makeStableName f >>= return . hashStableName
          liftIO $ modifyIORef rmap $ \mapth -> M.insert id tv mapth


-- | run an unique continuation for each connection. The first thread that execute `unique` is
-- executed for that connection. The rest are ignored.
unique :: TransIO a -> TransIO a
unique f= do
   Connection{closChildren=rmap} <- getSData <|> error "unique: only works within a connection. Use wormhole"
   mapth <- liftIO $ readIORef rmap
   id <- liftIO $ f `seq` makeStableName f >>= return .  hashStableName

   let mx = M.lookup id mapth
   case mx of
          Just _ -> empty
          Nothing -> do
             tv <- get
             liftIO $ modifyIORef rmap $ \mapth -> M.insert id tv mapth
             f 



-- | A wormhole opens a connection with another node anywhere in a computation.
-- `teleport` uses this connection to translate the computation back and forth between the two nodes connected
wormhole :: Loggable a => Node -> Cloud a -> Cloud a
wormhole node (Cloud comp) = local $ Transient $ do
   moldconn <- getData :: StateIO (Maybe Connection)
   mclosure <- getData :: StateIO (Maybe Closure)
      -- when (isJust moldconn) . setState $ ParentConnection (fromJust moldconn) mclosure
   
   -- labelState . fromString $ "wormhole" ++ show node
   Log rec _ _  _<- getData `onNothing` return (Log False [][] 0)

   if not rec                                    
            then runTrans $ (do
                    -- return () !> "NOT REC"
                    conn <-  mconnect node
                    liftIO $ writeIORef (remoteNode conn) $ Just node
                    setData  conn{synchronous= maybe False id $ fmap synchronous moldconn, calling= True}


                    setData  $ Closure 0

                    
                    comp )
                  <*** do
                       when (isJust moldconn) . setData $ fromJust moldconn
                       when (isJust mclosure) . setData $ fromJust mclosure
                    -- <** is not enough since comp may be reactive
            else do
                    -- return () !> "YES REC"
                    let conn = fromMaybe (error "wormhole: no connection in remote node") moldconn
                    setData $ conn{calling= False}
                    runTrans $ comp
                             <***  do when (isJust mclosure) . setData $ fromJust mclosure


#ifndef ghcjs_HOST_OS
type JSString= String
pack= id



#endif

data CloudException = CloudException Node IdClosure   String deriving (Typeable, Show, Read)

instance Exception CloudException 

-- | set remote invocations synchronous
-- this is necessary when data is transfered very fast from node to node in a stream non-deterministically
-- in order to keep the continuation of the calling node unchanged ultil the arrival of the response
-- since all the calls share a single continuation in the calling node.
--
-- If there is no response from the remote node, the streaming is interrupted
--
-- > main= keep $ initNode $  onBrowser $  do
-- >  local $ setSynchronous True 
-- >  line  <- local $  threads 0 $ choose[1..10::Int] 
-- >  localIO $ print ("1",line)
-- >  atRemote $ localIO $ print line 
-- >  localIO $ print ("2", line) 

setSynchronous :: Bool -> TransIO ()
setSynchronous sync= do
   modifyData'(\con -> con{synchronous=sync}) (error "setSynchronous: no communication data")
   return ()

-- set synchronous mode for remote calls within a cloud computation and also avoid unnecessary
-- thread creation
syncStream :: Cloud a -> Cloud a 
syncStream proc=  do
    sync <- local $ do
      Connection{synchronous= synchronous} <- modifyData'(\con -> con{synchronous=True}) err 
      return synchronous
    Cloud $ threads 0 $ runCloud' proc <***  modifyData'(\con -> con{synchronous=sync})  err
    where err= error "syncStream: no communication data"


teleport :: Cloud ()
teleport  =  local $ Transient $ do
     labelState $ fromString "teleport"
     cont <- get



     Log rec log fulLog closLocal <- getData `onNothing` return (Log False [][] 0)
    
     conn@Connection{connData=contype, synchronous=synchronous, localClosures= localClosures,calling= calling} <- getData
                             `onNothing` error "teleport: No connection defined: use wormhole"
     if not rec   -- !> ("teleport rec,loc fulLog=",rec,log,fulLog)
                  -- if is not recovering in the remote node then it is active
      then  do

        
        -- when a node call itself, there is no need of socket communications
        
        case contype of
         Just Self -> runTrans $ do
               setData  WasParallel
               abduce   !> "SELF" -- call himself
               liftIO $ do
                  remote <- readIORef $ remoteNode conn
                  writeIORef (myNode conn) $ fromMaybe (error "teleport: no connection?") remote
             

         _ -> do


         --read this Closure
          Closure closRemote  <- getData `onNothing`  return (Closure 0 )

          (closRemote',tosend) <- if closRemote == 0 
                      -- for localFix
                      then do
                         mf <-  getData 
                         case mf of
                          Just (service:: Bool,l :: Int ,cl :: Int ,rn :: IORef (M.Map Int Int)) -> do
                                 n <- liftIO $ atomicModifyIORef' rn $ \map -> 
                                           let n= (fromMaybe 0 $ M.lookup (idConn conn) map) 
                                           in (M.insert (idConn conn) (n+1) map, n)

                                 -- return () !> (fulLog,l,cl,n)
                                 
                                 return $ if n>0  then (cl,drop l $ reverse fulLog) 
                                          else if service then (0,drop l $ reverse fulLog)
                                          else (0, reverse fulLog)
                                 
                          _ -> return (0,reverse fulLog)
                         
                      else return (closRemote, reverse log)
          
          
       
          liftIO $ do 
            map <- readMVar localClosures
            let mr = M.lookup closLocal map
            pair <- case mr of
              Just (mvar,_) -> do when synchronous $ takeMVar mvar ; return (mvar,cont)
              _ -> do mv <- newEmptyMVar; return (mv,cont)

            modifyMVar_ localClosures $ \map ->  return $ M.insert closLocal pair map


          -- The log sent is in the order of execution. log is in reverse order
              
          -- send log with closure ids at head
          msend conn $ SMore $ ClosureData closRemote' closLocal tosend 
                                       !> ("teleport sending", SMore (unsafePerformIO $ readIORef $ remoteNode conn,closRemote',closLocal,tosend))
                                       !> "--------->------>---------->"
 
  

          return Nothing

      else do    -- needed closure 0 for unique services
        liftIO $ modifyMVar_ localClosures $ \map -> do
           case M.lookup 0 map  of
             Nothing -> return $ M.insert 0 (unsafePerformIO $ newEmptyMVar,  cont) map
             Just _  -> return map
        return $ Just ()
             
{- |
One problem of forwarding closures for streaming is that it could transport not only the data but extra information that reconstruct the closure in the destination node. In a single in-single out interaction It may not be a problem, but think, for example, when I have to synchronize N editors by forwarding small modifications, or worst of all, when transmitting packets of audio or video. But the size of the closure, that is, the amount of variables that I have to transport increases when the code is more complex. But transient build closures upon closures, so It has to send only what has changed since the last interaction.

In one-to-one interactions whithin a wormhole, this is automatic, but when there are different wormholes involved, it is necessary
to tell explicitly what is the closure that will continue the execution. this is what `localFix` does. otherwise it will use the closure 0.

> main= do
>      filename <- local input
>      source <- atServer $ local $ readFile filename
>      local $ render source inEditor
>     --  send upto here one single time please,  so I only stream the deltas
>      localFix
>      delta <- react  onEachChange
>      forallNodes $ update delta

if forwardChanges send to all the nodes editing the document, the data necessary to reconstruct the 
closure would include even the source code of the file on EACH change. 
Fortunately it is possible to fix a closure that will not change in all the remote nodes so after that, 
I only have to send the only necessary variable, the delta. This is as efficient as an hand-made 
socket write/forkThread/readSocket loop for each node.
-}
localFix=  localFixServ False


localFixServ isService = Cloud $ noTrans $ do
   Log rec log fulLog closLocal <- getData `onNothing` return (Log False [][] 0)
   if rec 
     then do
         conn@Connection{..} <- getData
                             `onNothing` error "teleport: No connection defined: use wormhole"
         cont <- get
         mv <- liftIO  newEmptyMVar
         liftIO $ modifyMVar_ localClosures $ \map ->  return $ M.insert closLocal (mv,cont) map

     else do
         ref <- liftIO $ newIORef M.empty
         setState  (isService,length fulLog,closLocal, ref :: IORef (M.Map Int Int))
   
{-
localFixServ1 isService = Cloud $ noTrans $ do
   Log rec log fulLog closLocal <- getData `onNothing` return (Log False [][] 0)
   if rec 
     then do
         conn@Connection{..} <- getData
                             `onNothing` error "teleport: No connection defined: use wormhole"
         cont <- get
         mv <- liftIO  newEmptyMVar
         liftIO $ modifyMVar_ localClosures $ \map ->  return $ M.insert closLocal (mv,cont) map

    
     else  
         let fillState= do
                (_ :: Bool,_ :: Int, _ :: Int, ref :: IORef (M.Map Int Int)) <- getRState
                setRState (isService,length fulLog,closLocal,ref)
               
             initState= do
                ref <- liftIO $ newIORef M.empty
                setRState (isService, length fulLog, closLocal, ref :: IORef (M.Map Int Int))

         in do
             runTrans $ fillState <|> initState
             return ()

initLocalFix = local $ do 
        ref <- liftIO $ newIORef M.empty
        setRState (False, 0:: Int, 0 :: Int, ref :: IORef (M.Map Int Int))
-}

-- | forward exceptions to the calling node
reportBack :: TransIO ()
reportBack= onException $ \(e :: SomeException) -> do 
    conn <- getData `onNothing` error "reportBack: No connection defined: use wormhole"
    Closure closRemote <- getData `onNothing` error "teleport: no closRemote"
    node <- getMyNode
    let msg= SError $ toException $ ErrorCall $  show $ show $ CloudException node closRemote $ show e
    msend conn msg  !> "MSEND"



-- | copy a session data variable from the local to the remote node.
-- If there is none set in the local node, The parameter is the default value.
-- In this case, the default value is also set in the local node.
copyData def = do
  r <- local getSData <|> return def
  onAll $ setData r
  return r


-- | write to the mailbox
-- Mailboxes are node-wide, for all processes that share the same connection data, that is, are under the
-- same `listen`  or `connect`
-- while EVars are only visible by the process that initialized  it and his children.
-- Internally, the mailbox is in a well known EVar stored by `listen` in the `Connection` state.
putMailbox :: Typeable val => val -> TransIO ()
putMailbox = putMailbox' (0::Int)

-- | write to a mailbox identified by an identifier besides the type
putMailbox' :: (Typeable key, Ord key, Typeable val) =>  key -> val -> TransIO ()
putMailbox'  idbox dat= do
   let name= MailboxId idbox $ typeOf dat
   Connection{comEvent= mv} <- getData `onNothing` errorMailBox
   mbs <- liftIO $ readIORef mv
   let mev =  M.lookup name mbs
   case mev of
     Nothing -> newMailbox name >> putMailbox' idbox dat
     Just ev -> writeEVar ev $ unsafeCoerce dat


newMailbox :: MailboxId -> TransIO ()
newMailbox name= do
--   return ()  -- !> "newMailBox"
   Connection{comEvent= mv} <- getData `onNothing` errorMailBox
   ev <- newEVar
   liftIO $ atomicModifyIORef mv $ \mailboxes ->   (M.insert name ev mailboxes,())


errorMailBox= error "MailBox: No connection open. Use wormhole"

-- | get messages from the mailbox that matches with the type expected.
-- The order of reading is defined by `readTChan`
-- This is reactive. it means that each new message trigger the execution of the continuation
-- each message wake up all the `getMailbox` computations waiting for it.
getMailbox :: Typeable val => TransIO val
getMailbox = getMailbox' (0 :: Int)

-- | read from a mailbox identified by an identifier besides the type
getMailbox' :: (Typeable key, Ord key, Typeable val) => key -> TransIO val
getMailbox' mboxid = x where
 x = do
   let name= MailboxId mboxid $ typeOf $ typeOf1 x
   Connection{comEvent= mv} <- getData `onNothing` errorMailBox
   mbs <- liftIO $ readIORef mv
   let mev =  M.lookup name mbs
   case mev of
     Nothing ->newMailbox name >> getMailbox' mboxid
     Just ev ->unsafeCoerce $ readEVar ev

 typeOf1 :: TransIO a -> a
 typeOf1 = undefined

-- | delete all subscriptions for that mailbox expecting this kind of data
cleanMailbox :: Typeable a => a -> TransIO ()
cleanMailbox = cleanMailbox'  (0 ::Int) 

-- | clean a mailbox identified by an Int and the type
cleanMailbox' :: (Typeable key, Ord key, Typeable a) => key ->  a -> TransIO ()
cleanMailbox'  mboxid witness= do
   let name= MailboxId mboxid $ typeOf witness
   Connection{comEvent= mv} <- getData `onNothing` error "getMailBox: accessing network events out of listen"
   mbs <- liftIO $ readIORef mv
   let mev =  M.lookup name mbs
   case mev of
     Nothing -> return()
     Just ev -> do cleanEVar ev
                   liftIO $ atomicModifyIORef mv $ \mbs -> (M.delete name mbs,())

-- | execute a Transient action in each of the nodes connected.
--
-- The response of each node is received by the invoking node and processed by the rest of the procedure.
-- By default, each response is processed in a new thread. To restrict the number of threads
-- use the thread control primitives.
--
-- this snippet receive a message from each of the simulated nodes:
--
-- > main = keep $ do
-- >    let nodes= map createLocalNode [2000..2005]
-- >    addNodes nodes
-- >    (foldl (<|>) empty $ map listen nodes) <|> return ()
-- >
-- >    r <- clustered $ do
-- >               Connection (Just(PortNumber port, _, _, _)) _ <- getSData
-- >               return $ "hi from " ++ show port++ "\n"
-- >    liftIO $ putStrLn r
-- >    where
-- >    createLocalNode n= createNode "localhost" (PortNumber n)
clustered :: Loggable a  => Cloud a -> Cloud a
clustered proc= callNodes (<|>) empty proc


-- A variant of `clustered` that wait for all the responses and `mappend` them
mclustered :: (Monoid a, Loggable a)  => Cloud a -> Cloud a
mclustered proc= callNodes (<>) mempty proc


callNodes op init proc= loggedc' $ do
    nodes <-  local getEqualNodes
    callNodes' nodes op init proc


callNodes' nodes op init proc= loggedc' $ foldr op init $ map (\node -> runAt node proc) nodes
-----
#ifndef ghcjs_HOST_OS
sendRaw (Connection _ _ _ (Just (Node2Web  sconn )) _ _ _ _ _ _ _) r=
      liftIO $   WS.sendTextData sconn  r                                  !> ("NOde2Web",r)

sendRaw (Connection _ _ _ (Just (Node2Node _ sock _)) _ _ blocked _ _ _ _) r=
      liftIO $  withMVar blocked $ const $  SBS.sendMany sock
                                      (BL.toChunks r )                    !> ("NOde2Node",r)

sendRaw (Connection _ _ _(Just (TLSNode2Node  ctx )) _ _ blocked _ _ _ _) r=
      liftIO $ withMVar blocked $ const $ sendTLSData ctx  r         !> ("TLNode2Web",r)

#else
sendRaw (Connection _ _ _ (Just (Web2Node sconn)) _ _ blocked _  _ _ _) r= liftIO $
   withMVar blocked $ const $ JavaScript.Web.WebSocket.send   r sconn     !> "MSEND SOCKET"
#endif

sendRaw _ _= error "No connection stablished"

type LengthFulLog= Int
data NodeMSG= ClosureData IdClosure IdClosure CurrentPointer 

   deriving (Typeable, Read, Show)

msend ::  MonadIO m => Connection -> StreamData NodeMSG -> m ()

msend (Connection _ _ _ (Just Self) _ _ _ _ _ _ _) r= return ()

#ifndef ghcjs_HOST_OS


msend (Connection _ _ _ (Just (Node2Node _ sock _)) _ _ blocked _ _ _ _) r=do
   liftIO $   withMVar blocked $  const $ SBS.sendAll sock $ BC.pack (show r)  -- !> ("N2N SEND", r)

msend (Connection _ _ _ (Just (TLSNode2Node ctx)) _ _ _ _ _ _ _) r=
     liftIO $ sendTLSData  ctx $ BS.pack (show r)                            --  !> "TLS SEND"


msend (Connection _ _ _ (Just (Node2Web sconn)) _ _ _ _ _ _ _) r=liftIO $
  {-withMVar blocked $ const $ -} WS.sendTextData sconn $ BS.pack (show r)    !> "websockets send"



#else

msend (Connection _ _ remoten (Just (Web2Node sconn)) _ _ blocked _  _ _ _) r= liftIO $  do
  -- when (js_readystate sconn /= 1) $ do  -- must try to reconnect
  --        Just node <- liftIO $ readIORef remoten
  --        ...
  withMVar blocked $ const $ JavaScript.Web.WebSocket.send  (JS.pack $ show r) sconn    !> "MSEND SOCKET"



#endif

msend (Connection _ _ _ Nothing _ _  _ _ _ _ _) _= error "msend out of connection context: use wormhole to connect"



mread :: Loggable a => Connection -> TransIO (StreamData a)


#ifdef ghcjs_HOST_OS


mread (Connection _ _ _ (Just (Web2Node sconn)) _ _ _ _  _ _ _)=  wsRead sconn



wsRead :: Loggable a => WebSocket  -> TransIO  a
wsRead ws= do
  dat <- react (hsonmessage ws) (return ())
  case JM.getData dat of
    JM.StringData str  ->  return (read' $ JS.unpack str)
                 !> ("Browser webSocket read", str)  !> "<------<----<----<------"
    JM.BlobData   blob -> error " blob"
    JM.ArrayBufferData arrBuffer -> error "arrBuffer"



wsOpen :: JS.JSString -> TransIO WebSocket
wsOpen url= do
   ws <-  liftIO $ js_createDefault url      --  !> ("wsopen",url)
   react (hsopen ws) (return ())             -- !!> "react"
   return ws                                 -- !!> "AFTER ReACT"

foreign import javascript safe
    "window.location.hostname"
   js_hostname ::    JSVal

foreign import javascript safe
   "window.location.pathname"
  js_pathname ::    JSVal

foreign import javascript safe
    "window.location.protocol"
   js_protocol ::    JSVal

foreign import javascript safe
   "(function(){var res=window.location.href.split(':')[2];if (res === undefined){return 80} else return res.split('/')[0];})()"
   js_port ::   JSVal

foreign import javascript safe
    "$1.onmessage =$2;"
   js_onmessage :: WebSocket  -> JSVal  -> IO ()


getWebServerNode :: TransIO Node
getWebServerNode = liftIO $ do
   h <- fromJSValUnchecked js_hostname
   p <- fromIntegral <$> (fromJSValUnchecked js_port :: IO Int)
   createNode h p


hsonmessage ::WebSocket -> (MessageEvent ->IO()) -> IO ()
hsonmessage ws hscb= do
  cb <- makeCallback1 MessageEvent hscb
  js_onmessage ws cb

foreign import javascript safe
             "$1.onopen =$2;"
   js_open :: WebSocket  -> JSVal  -> IO ()

foreign import javascript safe
             "$1.readyState"
  js_readystate ::  WebSocket -> Int

newtype OpenEvent = OpenEvent JSVal deriving Typeable
hsopen ::  WebSocket -> (OpenEvent ->IO()) -> IO ()
hsopen ws hscb= do
   cb <- makeCallback1 OpenEvent hscb
   js_open ws cb

makeCallback1 :: (JSVal -> a) ->  (a -> IO ()) -> IO JSVal

makeCallback1 f g = do
   Callback cb <- CB.syncCallback1 CB.ContinueAsync (g . f)
   return cb

-- makeCallback ::  IO () -> IO ()
makeCallback f  = do
  Callback cb <- CB.syncCallback CB.ContinueAsync  f
  return cb
   


foreign import javascript safe
   "new WebSocket($1)" js_createDefault :: JS.JSString -> IO WebSocket


#else
mread (Connection _ _ _ (Just (Node2Node _ _ _)) _ _ _ _ _ _ _) =  parallelReadHandler -- !> "mread"

mread (Connection _ _ _ (Just (TLSNode2Node _ )) _ _ _ _ _ _ _) =  parallelReadHandler
--        parallel $ do
--            s <- recvTLSData  ctx
--            return . read' $  BC.unpack s

mread (Connection _ _ _  (Just (Node2Web sconn )) _ _ _ _ _ _ _)=
        parallel $ do
            s <- WS.receiveData sconn
            return . read' $  BS.unpack s
                !>  ("WS MREAD RECEIVED ----<----<------<--------", s)


       



parallelReadHandler :: Loggable a => TransIO (StreamData a)
parallelReadHandler= do
      str <- giveData :: TransIO BS.ByteString

      r <- choose $ readStream  str

      return  r
                   !> ("parallel read handler read",  r)
                   !> "<-------<----------<--------<----------"
    where
    readStream :: (Typeable a, Read a) =>  BS.ByteString -> [StreamData a]
    readStream s=  readStream1 $ BS.unpack s
     where

     readStream1 s=
       let [(x,r)] = reads  s
       in  x : readStream1 r



getWebServerNode :: TransIO Node
getWebServerNode = getNodes >>= return . head
#endif



mclose :: Connection -> IO ()

#ifndef ghcjs_HOST_OS

mclose (Connection _ _ _
   (Just (Node2Node _  sock _ )) _ _ _ _ _ _ _)= NS.close sock

mclose (Connection _ _ _
   (Just (Node2Web sconn ))
   _ _ _ _  _ _ _)=
    WS.sendClose sconn ("closemsg" :: BS.ByteString)

#else

mclose (Connection _ _ _ (Just (Web2Node sconn)) _ _ blocked _ _ _ _)=
    JavaScript.Web.WebSocket.close Nothing Nothing sconn

#endif



mconnect :: Node -> TransIO  Connection
mconnect  node'=  do
  node <- fixNode node'
  nodes <- getNodes
  return ()                                                !>  ("mconnnect", nodePort node)
  let fnode =  filter (==node)  nodes
  case fnode of
   [] -> mconnect1 node   -- !> "NO NODE"
   [node'@(Node _ _ pool _)] -> do
      plist <- liftIO $  readMVar $ fromJust pool 
      case plist of                                      --  !>  ("length", length plist,nodePort node) of
        (handle:_) -> do
                  delData $ Closure undefined
                  return  handle
                                                         --      !>   ("REUSED!", node)
        _ ->do
             delNodes [node]
             mconnect1 node                                 
  where


#ifndef ghcjs_HOST_OS
  mconnect1 (node@(Node host port _ _))= do

     return ()  !> ("MCONNECT1",host,port,nodeServices node)
     (conn,parseContext) <- checkSelf node                                 <|>
                            timeout 1000000 (connectNode2Node host port)   <|>
                            timeout 1000000 (connectWebSockets host port)  <|> 
                            checkRelay                                     <|>
                            (throw $ ConnectionError "" node)

     setState conn
     setState parseContext

     -- write node connected in the connection
     liftIO $ writeIORef (remoteNode conn) $ Just node
     -- write connection in the node
     liftIO $ modifyMVar_ (fromJust $ connection node) . const $ return [conn]
     addNodes [node]

     case connData conn of
       Just Self -> return()
       _         -> watchConnection
     delData $ Closure undefined
     return  conn

    where
    checkSelf node= do
      node' <- getMyNode
      v <- liftIO $ readMVar (fromJust $ connection  node') -- to force connection in case of calling a service of itself
      if node /= node' ||   null v  
       then  empty
       else do
          conn<- case connection node of
             Nothing    -> error "checkSelf error"
             Just ref   ->  do
                 cnn <- getSData <|> error "checkself: no connection"
                 rnode  <- liftIO $ newIORef node'
                 conn   <- defConnection >>= \c -> return c{myNode= rnode, comEvent=comEvent cnn, connData= Just Self} !> "DEFF1"
                 liftIO $ withMVar ref $ const $ return [conn]
                 return conn

          return (conn,(ParseContext (error "checkSelf parse error") (error "checkSelf parse error")
                            ::  ParseContext BS.ByteString)) 

    timeout t proc=do
       r <- collect' 1 t proc
       case r of
          []  -> empty   -- !> "TIMEOUT EMPTY"
          r:_ -> return r     -- !> "RETURN COLLECT"

    checkRelay= do
        case lookup "relay" $ nodeServices node of
                    Nothing -> empty  -- !> "NO RELAY"
                    Just relayinfo -> do
                       let (h,p)= read relayinfo
                       connectWebSockets1  h p $    "/relay/"  ++  h  ++ "/" ++ show p ++ "/"
      
    noParseContext= (ParseContext (error "relay error") (error "relay error")
                             ::  ParseContext BS.ByteString)

    connectSockTLS host port= do
        return ()                                         !> "connectSockTLS"

        let size=8192
        Connection{myNode=my,comEvent= ev} <- getSData <|> error "connect: listen not set for this node"

        sock  <- liftIO $ connectTo'  size  host $ PortNumber $ fromIntegral port

        conn' <- defConnection >>= \c ->
                     return c{myNode=my, comEvent= ev,connData=
                     
                     Just $ (Node2Node u  sock (error $ "addr: outgoing connection"))} 

        setData conn'
        input <-  liftIO $ SBSL.getContents sock

        setData $ ParseContext (error "parse context: Parse error") input


        maybeClientTLSHandshake host sock input
        return () !> "After"


      `catcht` \(e :: SomeException) ->   empty 


    connectNode2Node host port= do
        return () !> "NODE 2 NODE"
        connectSockTLS host port
        return () !> "AFTER2"

        conn <- getSData <|> error "mconnect: no connection data"
        sendRaw conn "CLOS a b\r\n\r\n"
        return () !> "AFTER3"
        r <- liftIO $ readFrom conn

        case r of
          "OK" ->  do
                parseContext <- getState
                return (conn,parseContext)

          _    ->  do
               let Connection{connData=cdata}= conn
               case cdata of
                     Just(Node2Node _ s _) ->  liftIO $ NS.close s -- since the HTTP firewall closes the connection
--                   Just(TLSNode2Node c) -> contextClose c   -- TODO
               empty

    connectWebSockets host port = connectWebSockets1 host port "/"
    connectWebSockets1 host port verb= do
         return () !> "WEBSOCKETS"
         connectSockTLS host port  -- a new connection

         never  <- liftIO $ newEmptyMVar :: TransIO (MVar ())
         conn   <- getSData <|> error "connectWebSockets: no connection"
         stream <- liftIO $ makeWSStreamFromConn conn
         let hostport= host++(':': show port)
             headers= [] -- if verb =="/" then [("Host",fromString hostport)] else []
         wscon  <- react (NWS.runClientWithStream stream hostport verb
                         WS.defaultConnectionOptions headers) 
                         (takeMVar never)


         return (conn{connData=  Just $ (Node2Web wscon)}, noParseContext)

--    noConnection= error $ show node ++ ": no connection"


    watchConnection= do
        conn <- getSData
        parseContext <- getSData <|> error "NO PARSE CONTEXT"
                         :: TransIO (ParseContext BS.ByteString)
        chs <- liftIO $ newIORef M.empty
        let conn'= conn{closChildren= chs}
        -- liftIO $ modifyMVar_ (fromJust pool) $  \plist -> do
        --                  if not (null plist) then print "DUPLICATE" else return ()
        --                  return $ conn':plist    -- !> (node,"ADDED TO POOL")

        -- tell listenResponses to watch incoming responses
        putMailbox  ((conn',parseContext,node)                            
              :: (Connection,ParseContext BS.ByteString,Node))
        liftIO $ threadDelay 100000  -- give time to initialize listenResponses

#else
  mconnect1 (node@(Node host port (Just pool) _))= do
     conn' <- getSData <|> error "connect: listen not set for this node"
     if nodeHost node== "webnode" then return  conn'{connData= Just Self} else do
        ws <- connectToWS host $ PortNumber $ fromIntegral port
--                                                           !> "CONNECTWS"
        let conn=  conn'{connData= Just  (Web2Node ws)}
--                                                           !>  ("websocker CONNECION")
        let parseContext =
                      ParseContext (error "parsecontext not available in the browser")
                        ("" :: JSString)

        chs <- liftIO $ newIORef M.empty
        let conn'= conn{closChildren= chs}
        liftIO $ modifyMVar_ pool $  \plist -> return $ conn':plist
        putMailbox  (conn',parseContext,node)  -- tell listenResponses to watch incoming responses
        delData $ Closure undefined
        return  conn
#endif

  u= undefined

data ConnectionError= ConnectionError String Node deriving Show 

instance Exception ConnectionError

-- mconnect _ = empty


   

#ifndef ghcjs_HOST_OS
close1 sock= do

  NS.setSocketOption sock NS.Linger 0
  NS.close sock

connectTo' bufSize hostname (PortNumber port) =  do
        proto <- BSD.getProtocolNumber "tcp"
        bracketOnError
            (NS.socket NS.AF_INET NS.Stream proto)
            (sClose)  -- only done if there's an error
            (\sock -> do
              NS.setSocketOption sock NS.RecvBuffer bufSize
              NS.setSocketOption sock NS.SendBuffer bufSize
 


--              NS.setSocketOption sock NS.SendTimeOut 1000000  !> ("CONNECT",port)

              he <- BSD.getHostByName hostname

              NS.connect sock (NS.SockAddrInet port (BSD.hostAddress he))

              return sock)

#else
connectToWS  h (PortNumber p) = do
   protocol <- liftIO $ fromJSValUnchecked js_protocol
   pathname <- liftIO $ fromJSValUnchecked js_pathname
   return () !> ("PAHT",pathname)
   let ps = case (protocol :: JSString)of "http:" -> "ws://"; "https:" -> "wss://"
   wsOpen $ JS.pack $ ps++ h++ ":"++ show p ++ pathname
#endif



type Blocked= MVar ()
type BuffSize = Int
data ConnectionData=
#ifndef ghcjs_HOST_OS
                   Node2Node{port :: PortID
                            ,socket ::Socket
                            ,sockAddr :: NS.SockAddr
                             }
                   | TLSNode2Node{tlscontext :: SData}
                   | Node2Web{webSocket :: WS.Connection}
--                   | WS2Node{webSocketNode :: WS.Connection}
                   | Self

#else
                   Self
                   | Web2Node{webSocket :: WebSocket}
#endif
   --   deriving (Eq,Ord)


data MailboxId =  forall a .(Typeable a, Ord a) => MailboxId a TypeRep

instance Eq MailboxId where
   id1 == id2 =  id1 `compare` id2== EQ

instance Ord MailboxId where
   MailboxId n t `compare` MailboxId n' t'=
     case typeOf n `compare` typeOf n' of
         EQ -> case n `compare` unsafeCoerce n' of
                 EQ -> t `compare` t'
                 LT -> LT
                 GT -> GT

         other -> other

data Connection= Connection{idConn     :: Int
                           ,myNode     :: IORef Node
                           ,remoteNode :: IORef (Maybe Node)
                           ,connData   :: Maybe ConnectionData
                           ,bufferSize :: BuffSize
                           -- Used by getMailBox, putMailBox
                           ,comEvent   :: IORef (M.Map MailboxId (EVar SData))
                           -- multiple wormhole/teleport use the same connection concurrently
                           ,blocked    :: Blocked
                           ,calling    :: Bool
                           ,synchronous :: Bool
                           -- local localClosures with his log and his continuation
                           ,localClosures   :: MVar (M.Map IdClosure  (MVar (),EventF))

                           -- for each remote closure that points to local closure 0,
                           -- a new container of child processes
                           -- in order to treat them separately
                           -- so that 'killChilds' do not kill unrelated processes
                           ,closChildren :: IORef (M.Map Int EventF)}

                  deriving Typeable









defConnection :: (MonadIO m, MonadState EventF m)  => m Connection


-- #ifndef ghcjs_HOST_OS
defConnection =  do
  idc <- genGlobalId
  liftIO $ do
    my <- newIORef (error "node in default connection")
    x <- newMVar ()
    y <- newMVar M.empty
    noremote <- newIORef Nothing
    z <-   newIORef M.empty
    return $ Connection idc my noremote Nothing  8192
                  (error "defConnection: accessing network events out of listen")
                  x  False False y z



#ifndef ghcjs_HOST_OS
setBuffSize :: Int -> TransIO ()
setBuffSize size= Transient $ do
   conn<- getData `onNothing`  (defConnection !> "DEFF3")
   setData $ conn{bufferSize= size}
   return $ Just ()

getBuffSize=
  (do getSData >>= return . bufferSize) <|> return  8192


-- | Setup the node to start listening for incoming connections.
--
listen ::  Node ->  Cloud ()
listen  (node@(Node _   port _ _ )) = onAll $ do
--   onException $ \(e :: SomeException) -> do
--         case fromException e of
--           Just (CloudException _ _ _) -> return()
--           _ -> do
--                      Closure closRemote <- getData `onNothing` error "teleport: no closRemote"
--                      conn <- getData `onNothing` error "reportBack: No connection defined: use wormhole"
--                      msend conn  $ SError $ toException $ ErrorCall $ show $ show $ CloudException node closRemote $ show e

   ex <- exceptionPoint :: TransIO (BackPoint SomeException)
   setData ex
   addThreads 1
   
   setData $ Log False [] [] 0

   conn' <- getSData <|> defConnection
   ev <- liftIO $ newIORef M.empty
   chs <- liftIO $ newIORef M.empty
   let conn= conn'{connData=Just Self, comEvent=ev,closChildren=chs}
   pool <- liftIO $ newMVar [conn]

   let node'= node{connection=Just pool}
   liftIO $ writeIORef (myNode conn) node'
   setData conn

   liftIO $ modifyMVar_ (fromJust $ connection node') $ const $ return [conn]

   addNodes [node'] 
   setRState(JobGroup M.empty)   --used by resetRemote

   mlog <- listenNew (fromIntegral port) conn   <|> listenResponses :: TransIO (StreamData NodeMSG)
   execLog mlog



-- listen incoming requests
listenNew port conn'=  do


   liftIO $ do
     putStr "listen at port: " 
     print port

   sock <- liftIO . listenOn $ PortNumber port

  
   liftIO $ do
      let bufSize= bufferSize conn'
      NS.setSocketOption sock NS.RecvBuffer bufSize
      NS.setSocketOption sock NS.SendBuffer bufSize

   -- wait for connections. One thread per connection
   (sock,addr) <- waitEvents $ NS.accept sock

   chs <- liftIO $ newIORef M.empty
--   case addr of
--     NS.SockAddrInet port host -> liftIO $ print("connection from", port, host)
--     NS.SockAddrInet6  a b c d -> liftIO $ print("connection from", a, b,c,d)
   noNode <- liftIO $ newIORef Nothing
   id1 <- genId
   let conn= conn'{idConn=id1,closChildren=chs, remoteNode= noNode}

   input <-  liftIO $ SBSL.getContents sock
   return () !> "SOME INPUT"
   -- cutExceptions

  --  onException $ \(e :: IOException) -> 
  --         when (ioeGetLocation e=="Network.Socket.recvBuf") $ do
  --            liftIO $ putStr "listen: " >> print e
             
  --            let Connection{remoteNode=rnode,localClosures=localClosures,closChildren= rmap} = conn
  --            -- TODO How to close Connection by discriminating exceptions
  --            mnode <- liftIO $ readIORef rnode
  --            case mnode of
  --              Nothing -> return ()
  --              Just node  -> do
  --                            liftIO $ putStr "removing1 node: " >> print node
  --                            nodes <- getNodes
  --                            setNodes $ nodes \\ [node]
  --            liftIO $ do
  --                 modifyMVar_ localClosures $ const $ return M.empty
  --                 writeIORef rmap M.empty
  --            -- topState >>= showThreads
            
  --            killBranch
             

   setData $ (ParseContext (NS.close sock >> error "Communication error" ) input
             ::ParseContext BS.ByteString)

   setState conn{connData=Just (Node2Node (PortNumber port) sock addr)}
   maybeTLSServerHandshake sock input



  --  (method,uri, headers) <- receiveHTTPHead
   (method, uri, vers) <- getFirstLine
   return () !> (method, uri, vers)
   case method of

     "CLOS" ->
          do
           conn <- getSData
           sendRaw conn "OK"                                   !> "CLOS detected"

           mread conn

     _ -> do   
           let uri'= BC.tail $ uriPath uri !> uriPath uri
           if  "api/" `BC.isPrefixOf` uri'
             then do

               log <- return $ Exec: (Var $ IDyns $ BS.unpack method):(map (Var . IDyns ) $ split $ BC.unpack $ BC.drop 4 uri')


               str <-  giveData  <|> error "no api data"
               headers <- getHeaders
               maybeSetHost headers
               log' <- case (method,lookup "Content-Type" headers) of
                       ("POST",Just "application/x-www-form-urlencoded") -> do
                            len <- read <$> BC.unpack
                                        <$> (Transient $ return (lookup "Content-Length" headers))
                            setData $ ParseContext (return SDone) $ BS.take len str

                            postParams <- parsePostUrlEncoded  <|> return []
                            return $ log ++  [(Var . IDynamic $ postParams)]

                       _ -> return $ log  -- ++ [Var $ IDynamic  str]

               return $ SMore $ ClosureData 0 0  log'

             else if "relay/"  `BC.isPrefixOf` uri' then proxy sock method vers uri'
                
             else do
                   headers <- getHeaders
                   maybeSetHost headers

                   return () !> (method,uri')
                   -- stay serving pages until a websocket request is received
                   servePages (method, uri', headers)
                   conn <- getSData 
                   sconn <- makeWebsocketConnection conn uri headers
                   -- websockets mode
                   rem <- liftIO $ newIORef Nothing
                   chs <- liftIO $ newIORef M.empty
                   cls <- liftIO $ newMVar M.empty
                   cme <- liftIO $ newIORef M.empty

                   let conn'= conn{connData= Just (Node2Web sconn)
                             , closChildren=chs,localClosures=cls, remoteNode=rem} --,comEvent=cme}
                   setState conn'    !> "WEBSOCKETS CONNECTION"

--                   async (return (SMore (0,0,[Exec]))) <|> do
                   do
--                     return ()                  !> "WEBSOCKET"
                    --  onException $ \(e :: SomeException) -> do
                    --     liftIO $ putStr "listen websocket:" >> print e
                    --     -- liftIO $ mclose conn'
                    --     -- killBranch
                    --     -- empty
                     -- ex <- exceptionPoint :: TransIO (BackPoint SomeException)
                     -- setData ex
                     r <-  parallel $ do
                             msg <- WS.receiveData sconn
                             return ()   !> ("Server WebSocket msg read",msg)
                                         !> "<-------<---------<--------------"

                             case reads $ BS.unpack msg of
                               [] -> do
                                   let log =Exec: [Var $ IDynamic  (msg :: BS.ByteString)]
                                   return $ SMore (ClosureData 0 0 log)
                               ((x ,_):_) -> return (x :: StreamData NodeMSG) -- StreamData (Int,Int,[LogElem]))

                     case r of
                       SError e -> do
--                           liftIO $ WS.sendClose sconn ("error" :: BS.ByteString)
                           back e
                       _ -> return r

     where
      uriPath = BC.dropWhile (/= '/')
      split []= []
      split ('/':r)= split r
      split s=
          let (h,t) = span (/= '/') s
          in h: split  t
      
      -- reverse proxy for urls that look like http://host:port/relay/otherhost/otherport/
      proxy sclient method vers uri' = do
        let (host:port:_)=  split $ BC.unpack $ BC.drop 6 uri'
        return () !> ("RELAY TO",host, port)
        --liftIO $ threadDelay 1000000
        sserver <- liftIO $ connectTo' 4096 host $ PortNumber $ fromIntegral $ read port
        return () !> "CONNECTED"
        rawHeaders <- getRawHeaders
        return () !>  ("RAWHEADERS",rawHeaders)
        let uri= BS.fromStrict $ let d x= BC.tail $ BC.dropWhile (/= '/') x in d . d $ d uri'
        
        let sent=   method <> BS.pack " /" 
                           <> uri  
                           <> BS.cons ' ' vers 
                           <> BS.pack "\r\n" 
                           <> rawHeaders <> BS.pack "\r\n\r\n"
        return () !> ("SENT",sent)
        liftIO $ SBSL.send  sserver sent
          -- Connection{connData=Just (Node2Node _ sclient _)} <- getState <|> error "proxy: no connection"
         

        (send sclient sserver <|> send sserver sclient)
            `catcht` \(e:: SomeException ) -> liftIO $ do 
                            putStr "Proxy: " >> print e
                            sClose sserver
                            sClose sclient
                            empty
                    
        empty
        where
        send f t= async $ mapData f t
        mapData from to = do
            content <- recv from 4096 
            return () !> (" proxy received ", content)
            if not $ BC.null content 
              then sendAll to content >> mapData from to
              else finish
            where
            finish=  sClose from >> sClose to
           -- throw $ Finish "finish"
           

      maybeSetHost headers= do
        setHost <- liftIO $ readIORef rsetHost
        when setHost $ do

          mnode <- liftIO $ do
           let mhost= lookup "Host" headers
           case mhost of
              Nothing -> return Nothing
              Just host -> atomically $ do
                   -- set the firt node (local node) as is called from outside
                     nodes <- readTVar  nodeList
                     let (host1,port)= BC.span (/= ':') host
                         hostnode= (head nodes){nodeHost=  BC.unpack host1
                                           ,nodePort= if BC.null port then 80
                                            else read $ BC.unpack $ BC.tail port}
                     writeTVar nodeList $ hostnode : tail nodes
                     return $ Just  hostnode  -- !> (host1,port)

          when (isJust mnode) $ do
            conn <- getState
            liftIO $ writeIORef (myNode conn) $fromJust mnode
          liftIO $ writeIORef rsetHost False  -- !> "HOSt SET"

{-#NOINLINE rsetHost #-}
rsetHost= unsafePerformIO $ newIORef True



--instance Read PortNumber where
--  readsPrec n str= let [(n,s)]=   readsPrec n str in [(fromIntegral n,s)]


--deriving instance Read PortID
--deriving instance Typeable PortID
#endif

type SystemString=
#ifndef ghcjs_HOST_OS 
       BS.ByteString
#else
       JSString
#endif

listenResponses :: Loggable a => TransIO (StreamData a)
listenResponses= do
      (conn, parsecontext, node) <- getMailbox   :: TransIO (Connection,ParseContext SystemString,Node)
      labelState . fromString $ "listen from: "++ show node

      setData conn
      


      setData (parsecontext :: ParseContext SystemString)






      -- cutExceptions
 --     onException $ \(e:: SomeException) -> do
--          liftIO $ putStr "ListenResponses: " >> print e
--          liftIO $ putStr "removing node: " >> print node
--          nodes <- getNodes
--          setNodes $ nodes \\ [node]
--          --  topState >>= showThreads
--          killChilds
--          let Connection{localClosures=localClosures}= conn
--          liftIO $ modifyMVar_ localClosures $ const $ return M.empty
--          empty


      mread conn




type IdClosure= Int

-- The remote closure ids for each node connection
newtype Closure= Closure  IdClosure  deriving (Read,Show,Typeable)





type RemoteClosure=  (Node, IdClosure)


newtype JobGroup= JobGroup  (M.Map BC.ByteString RemoteClosure) deriving Typeable

-- | if there is a remote job  identified by th string identifier, it stop that job, and set the
-- current remote operation (if any) as the current remote job for this identifier.
-- The purpose is to have a single remote job.
--  to identify the remote job, it should be used after the `wormhole` and before the remote call:
--
-- > r <- wormhole node $ do
-- >        stopRemoteJob "streamlog"
-- >        atRemote myRemotejob
--
-- So:
--
-- > runAtUnique ident node job= wormhole node $ do stopRemoteJob ident; atRemote job

-- This program receive a stream of "hello" from a second node when the option "hello" is entered in the keyboard
-- If you enter  "world", the "hello" stream from the second node
-- will stop and will print an stream of "world" from a third node:
-- Entering "hello" again will stream "hello" again from the second node and so on:


-- > main= keep $ initNode $ inputNodes <|> do
-- > 
-- >     local $ option "init" "init"
-- >     nodes <- local getNodes
-- >     r <- proc (nodes !! 1) "hello" <|> proc (nodes !! 2) "world"
-- >     localIO $ print r
-- >     return ()
-- > 
-- > proc node par = do
-- >   v <- local $ option par par
-- >   runAtUnique "name" node $ local $ do
-- >    abduce
-- >    r <- threads 0 $ choose $ repeat v
-- >    liftIO $ threadDelay 1000000 
-- >    return r

-- the nodes could be started from the command line as such in different terminals:

-- > program -p start/localhost/8000
-- > program -p start/localhost/8002
-- > program -p start/localhost/8001/add/localhost/8000/n/add/localhost/8002/n/init

-- The third is the one wich has the other two connected and can execute the two options.

stopRemoteJob :: BC.ByteString -> Cloud ()

stopRemoteJob ident =  do
    resetRemote ident
    Closure closr <-  local $ getData `onNothing` error "stopRemoteJob: Connection not set, use wormhole"
    return () !> ("CLOSRRRRRRRR", closr)
    fixClosure
    local $ do
      Closure closr <- getData `onNothing` error "stopRemoteJob: Connection not set, use wormhole"
      conn <- getData `onNothing` error "stopRemoteJob: Connection not set, use wormhole"
      remote <- liftIO $ readIORef $ remoteNode conn
      return (remote,closr) !> ("REMOTE",remote)
      
      JobGroup map  <- getRState <|> return (JobGroup M.empty) 
      setRState $ JobGroup $ M.insert ident (fromJust remote,closr) map



-- kill the remote job. Usually, before starting a new one.

resetRemote :: BC.ByteString -> Cloud ()

resetRemote ident =   do
    mj <- local $  do
      JobGroup map <- getRState <|> return (JobGroup M.empty)
      return $ M.lookup ident map

    when (isJust mj) $  do
        let (remote,closr)= fromJust mj
        --do   -- when (closr /= 0) $ do
        runAt remote $ local $ do
              conn@Connection {localClosures=localClosures} <- getData `onNothing` error "Listen: myNode not set"
              mcont <- liftIO $ modifyMVar localClosures $ \map -> return ( M.delete closr map,  M.lookup closr map)
              case mcont of
                Nothing -> error $ "closure not found: " ++ show closr
                Just (_,cont) -> do 
                                topState >>= showThreads 
                                liftIO $  killBranch' cont   
                                return ()

                        
   
execLog :: StreamData NodeMSG -> TransIO ()
execLog  mlog =Transient $ do
       
       case mlog of
             SError e -> do
               return() !> ("SERROR",e)
               case fromException e of
                 Just (ErrorCall str) -> do

                  case read str of
                    (e@(CloudException  _ closl   err)) -> do

                      process  closl (error "closr: should not be used") (Left  e) True
                 

             SDone   -> runTrans(back $ ErrorCall "SDone") >> return Nothing   -- TODO remove closure?
             SMore (ClosureData closl closr  log) -> process closl closr  (Right log) False
             SLast (ClosureData closl closr  log) -> process closl closr  (Right log) True
   where
   process :: IdClosure -> IdClosure  -> (Either CloudException CurrentPointer) -> Bool -> StateIO (Maybe ())
   process  closl closr  mlog  deleteClosure= do

      conn@Connection {localClosures=localClosures} <- getData `onNothing` error "Listen: myNode not set"
      if closl== 0 then do
       
       case mlog of
        Left except -> do
          setData $ Log True [] []

          return () !> "Exception received from network 1"
          runTrans $ throwt except
          empty
        Right log -> do
           setData $ Log True log  (reverse log) 0  
           setData $ Closure  closr
           
           
           return $ Just ()                  --  !> "executing top level closure"
       else do
         mcont <- liftIO $ modifyMVar localClosures
                         $ \map -> return (if deleteClosure then
                                           M.delete closl map
                                         else map, M.lookup closl map)
                                           -- !> ("localClosures=", M.size map)
         case mcont  of
           Nothing -> do
--
--              if closl == 0   -- add what is after execLog as closure 0
--               then do
--                     setData $ Log True log  $ reverse log
--                     setData $ Closure closr
--                     cont <- get    !> ("CLOSL","000000000")
--                     liftIO $ modifyMVar localClosures
--                            $ \map -> return (M.insert closl ([],cont) map,())
--                     return $ Just ()     --exec what is after execLog (closure 0)
--
--               else do
                     msend conn $ SLast (ClosureData closr closl  [])
                        -- to delete the remote closure
                     liftIO $ error ("request received for non existent closure: "
                                             ++  show closl)
           -- execute the closure
           Just (mv,cont) -> do 
 
              liftIO $ tryPutMVar mv () 
              liftIO $ runStateT (case mlog of
                Right log -> do
                  Log _ _ fulLog hash <- getData `onNothing` return (Log True [] [] 0)
                  -- return() !> ("fullog in execlog", reverse fulLog)
                  let nlog= reverse log ++  fulLog
                  
                  setData $ Log True  log  nlog  hash
                  setData $ Closure  closr 
                  setData NoRemote
                  -- return () !>  "RUNCONTINUATION"
                  runContinuation cont ()

                Left except -> do
                  setData $ Log True  []  []

                  return () !> "Exception received from the network 2"
                  Backtrack b stack <- getData `onNothing` error "NO STACK" :: StateIO (Backtrack SomeException)
                  return() !> ("LENGTH STACK", length stack)
                  runTrans $ throwt except) cont
  
              return Nothing
                            

#ifdef ghcjs_HOST_OS
listen node = onAll $ do
        addNodes [node]
        setRState(JobGroup M.empty)
        ex <- exceptionPoint :: TransIO (BackPoint SomeException)
        setData ex

        events <- liftIO $ newIORef M.empty
        rnode  <- liftIO $ newIORef node
        conn <-  defConnection >>= \c -> return c{myNode=rnode,comEvent=events}
        setData conn
        r <- listenResponses
        execLog  r
#endif

type Pool= [Connection]
type Package= String
type Program= String
type Service= [(Package, Program)]





--------------------------------------------


#ifndef ghcjs_HOST_OS


--    maybeRead line= unsafePerformIO $ do
--         let [(v,left)] = reads  line
----         print v
--         (v   `seq` return [(v,left)])
--                        `catch` (\(e::SomeException) -> do
--                          liftIO $ print  $ "******readStream ERROR in: "++take 100 line
--                          maybeRead left)


readFrom Connection{connData= Just(TLSNode2Node ctx)}= recvTLSData ctx

readFrom Connection{connData= Just(Node2Node _ sock _)} =  toStrict <$> loop

  where
  bufSize= 4098
  loop :: IO BL.ByteString
  loop = unsafeInterleaveIO $ do
    s <- SBS.recv sock bufSize

    if BC.length s < bufSize
      then  return $ BLC.Chunk s mempty
      else BLC.Chunk s `liftM` loop
      
readFrom _ = error "readFrom error"

toStrict= B.concat . BS.toChunks

makeWSStreamFromConn conn= do
     let rec= readFrom conn
         send= sendRaw conn
     makeStream                  -- !!> "WEBSOCKETS request"
            (do
                bs <-  rec         -- SBS.recv sock 4098
                return $ if BC.null bs then Nothing else Just  bs)
            (\mbBl -> case mbBl of
                Nothing -> return ()
                Just bl ->  send bl) -- SBS.sendMany sock (BL.toChunks bl) >> return())   -- !!> show ("SOCK RESP",bl)

makeWebsocketConnection conn uri headers= liftIO $ do

         stream <- makeWSStreamFromConn conn
         let
             pc = WS.PendingConnection
                { WS.pendingOptions     = WS.defaultConnectionOptions
                , WS.pendingRequest     = NWS.RequestHead  uri  headers False -- RequestHead (BC.pack $ show uri)
                                                      -- (map parseh headers) False
                , WS.pendingOnAccept    = \_ -> return ()
                , WS.pendingStream      = stream
                }


         sconn    <- WS.acceptRequest pc               -- !!> "accept request"
         WS.forkPingThread sconn 30
         return sconn

servePages (method,uri, headers)   = do
--   return ()                        !> ("HTTP request",method,uri, headers)
   conn <- getSData <|> error " servePageMode: no connection"

   if isWebSocketsReq headers
     then  return ()



     else do

        let file= if BC.null uri then "index.html" else uri

        {- TODO rendering in server
           NEEDED:  recodify View to use blaze-html in server. wlink to get path in server
           does file exist?
           if exist, send else do
              store path, execute continuation
              get the rendering
              send trough HTTP
           - put this logic as independent alternative programmer options
              serveFile dirs <|> serveApi apis <|> serveNode nodeCode
        -}
        mcontent <- liftIO $ (Just <$> BL.readFile ( "./static/out.jsexe/"++ BC.unpack file) )
                                `catch` (\(e:: SomeException) -> return Nothing)
                                
--                                    return  "Not found file: index.html<br/> please compile with ghcjs<br/> ghcjs program.hs -o static/out")
        case mcontent of
          Just content -> liftIO $ sendRaw conn $
            "HTTP/1.0 200 OK\r\nContent-Type: text/html\r\nConnection: close\r\nContent-Length: "
            <> BS.pack (show $ BL.length content) <>"\r\n\r\n" <> content

          Nothing ->liftIO $ sendRaw conn $ BS.pack $ 
              "HTTP/1.0 404 Not Found\nContent-Length: 13\nConnection: close\n\nNot Found 404"
        empty


--counter=  unsafePerformIO $ newMVar 0
api :: TransIO BS.ByteString -> Cloud ()
api  w= Cloud  $ do
   conn <- getSData  <|> error "api: Need a connection opened with initNode, listen, simpleWebApp"
   let send= sendRaw conn
   r <- w
   send r                         --  !> r









isWebSocketsReq = not  . null
    . filter ( (== mk "Sec-WebSocket-Key") . fst)


data HTTPMethod= GET | POST deriving (Read,Show,Typeable)

getFirstLine=  (,,) <$> getMethod <*> (toStrict <$> getUri) <*> getVers
    where
    getMethod= parseString
    getUri= parseString
    getVers= parseString

getRawHeaders=  dropSpaces >> (withData $ \s -> return $ scan mempty s)
 
   where
   scan  res str
         
       | "\r\n\r\n" `BS.isPrefixOf` str   = (res, BS.drop 4 str)
       | otherwise=  scan ( BS.snoc res $ BS.head str) $ BS.tail str 
  --  line= do
  --   dropSpaces
  --   tTakeWhile (not . endline)

type PostParams = [(BS.ByteString, String)]

parsePostUrlEncoded :: TransIO PostParams
parsePostUrlEncoded=  do
   dropSpaces
   many $ (,) <$> param  <*> value
   where
   param= tTakeWhile' ( /= '=')
   value= unEscapeString <$> BS.unpack <$> tTakeWhile' ( /= '&')




getHeaders =  manyTill paramPair  (string "\r\n\r\n")          -- !>  (method, uri, vers)

  where  


  paramPair=  (,) <$> (mk <$> getParam) <*> getParamValue
  

  getParam= do
      dropSpaces
      r <- tTakeWhile (\x -> x /= ':' && not (endline x))
      if BS.null r || r=="\r"  then  empty  else  anyChar >> return (toStrict r)
      where
      endline c= c== '\r' || c =='\n'

  getParamValue= toStrict <$> ( dropSpaces >> tTakeWhile  (\x -> not (endline x)))
      where
      endline c= c== '\r' || c =='\n'



#endif



#ifdef ghcjs_HOST_OS
isBrowserInstance= True
api _= empty
#else
-- | Returns 'True' if we are running in the browser.
isBrowserInstance= False

#endif





{-# NOINLINE emptyPool #-}
emptyPool :: MonadIO m => m (MVar Pool)
emptyPool= liftIO $ newMVar  []


-- | Create a node from a hostname (or IP address), port number and a list of
-- services.
createNodeServ ::  HostName -> Int -> Service -> IO Node
createNodeServ h p svs=  return $ Node h  p Nothing svs


createNode :: HostName -> Int -> IO Node
createNode h p= createNodeServ h p []

createWebNode :: IO Node
createWebNode= do
  pool <- emptyPool
  port <- randomIO
  return $ Node "webnode"  port (Just pool)  [("webnode","")]


instance Eq Node where
    Node h p _ _ ==Node h' p' _ _= h==h' && p==p'


instance Show Node where
    show (Node h p _ servs )= show (h,p, servs)

instance Read Node where
    readsPrec n s=
          let r= readsPrec n s
          in case r of
            [] -> []
            [((h,p,ss),s')] ->  [(Node h p Nothing ss ,s')]
          
          


-- inst    ghc-options: -threaded -rtsopts

nodeList :: TVar  [Node]
nodeList = unsafePerformIO $ newTVarIO []

deriving instance Ord PortID

--myNode :: Int -> DBRef  MyNode
--myNode= getDBRef $ key $ MyNode undefined

errorMyNode f= error $ f ++ ": Node not set. initialize it with connect, listen, initNode..."

-- | Return the local node i.e. the node where this computation is running.
getMyNode ::  TransIO Node -- (MonadIO m, MonadState EventF m) => m Node
getMyNode =  do
    Connection{myNode= node} <- getSData <|> errorMyNode "getMyNode"  :: TransIO Connection
    liftIO $ readIORef node

-- | Return the list of nodes in the cluster.
getNodes :: MonadIO m => m [Node]
getNodes  = liftIO $ atomically $ readTVar  nodeList


-- | get the nodes that have the same service definition that the calling node
getEqualNodes = do
    nodes <- getNodes

    let srv= nodeServices $ head nodes
    case srv of
      [] -> return $ filter (null . nodeServices) nodes 

      (srv:_)  -> return $ filter (\n ->  (not $ null $ nodeServices n) && head (nodeServices n) == srv  ) nodes

getWebNodes :: MonadIO m => m [Node]
getWebNodes = do
    nodes <- getNodes
    return $ filter ( (==) "webnode" . nodeHost) nodes
    
matchNodes f = do
      nodes <- getNodes
      return $ map (\n -> filter f $ nodeServices n) nodes 

-- | Add a list of nodes to the list of existing nodes know locally.
addNodes :: [Node] ->  TransIO () -- (MonadIO m, MonadState EventF m) => [Node] -> m ()
addNodes   nodes=  do
--  my <- getMyNode    -- mynode must be first
  nodes' <- mapM fixNode nodes
  liftIO . atomically $ do
    prevnodes <- readTVar nodeList
    writeTVar nodeList $  nub $ prevnodes ++ nodes'

delNodes nodes= liftIO $ atomically $ do
  nodes' <-  readTVar nodeList
  writeTVar nodeList $ nodes' \\ nodes

fixNode n= case connection n of
  Nothing -> do
      pool <- emptyPool
      return n{connection= Just pool}
  Just _ -> return n

-- | set the list of nodes
setNodes nodes= liftIO $ atomically $ writeTVar nodeList $  nodes


-- | Shuffle the list of cluster nodes and return the shuffled list.
shuffleNodes :: MonadIO m => m [Node]
shuffleNodes=  liftIO . atomically $ do
  nodes <- readTVar nodeList
  let nodes'= tail nodes ++ [head nodes]
  writeTVar nodeList nodes'
  return nodes'

--getInterfaces :: TransIO TransIO HostName
--getInterfaces= do
--   host <- logged $ do
--      ifs <- liftIO $ getNetworkInterfaces
--      liftIO $ mapM_ (\(i,n) ->putStrLn $ show i ++ "\t"++  show (ipv4 n) ++ "\t"++name n)$ zip [0..] ifs
--      liftIO $ putStrLn "Select one: "
--      ind <-  input ( < length ifs)
--      return $ show . ipv4 $ ifs !! ind




-- #ifndef ghcjs_HOST_OS
--instance Read NS.SockAddr where
--    readsPrec _ ('[':s)=
--       let (s',r1)= span (/=']')  s
--           [(port,r)]= readsPrec 0 $ tail $ tail r1
--       in [(NS.SockAddrInet6 port 0 (IP.toHostAddress6 $  read s') 0, r)]
--    readsPrec _ s=
--       let (s',r1)= span(/= ':') s
--           [(port,r)]= readsPrec 0 $ tail r1
--       in [(NS.SockAddrInet port (IP.toHostAddress $  read s'),r)]
-- #endif

-- | add this node to the list of know nodes in the remote node connected by a `wormhole`.
--  This is useful when the node is called back by the remote node.
-- In the case of web nodes with webSocket connections, this is the way to add it to the list of
-- known nodes in the server.
addThisNodeToRemote= do
    n <- local getMyNode
    atRemote $ local $ do
      n' <- setConnectionIn n
      addNodes [n']
    
setConnectionIn node=do 
    conn <- getState <|> error "addThisNodeToRemote: connection not found"
    ref <- liftIO $ newMVar [conn]
    return node{connection=Just ref}

-- | Add a node (first parameter) to the cluster using a node that is already
-- part of the cluster (second parameter).  The added node starts listening for
-- incoming connections and the rest of the computation is executed on this
-- newly added node.
connect ::  Node ->  Node -> Cloud ()
#ifndef ghcjs_HOST_OS
connect  node  remotenode =   do
    listen node <|> return ()
    connect' remotenode



-- | Reconcile the list of nodes in the cluster using a remote node already
-- part of the cluster. Reconciliation end up in each node in the cluster
-- having  the same list of nodes.
connect' :: Node -> Cloud ()
connect'  remotenode= loggedc $ do
    nodes <- local getNodes
    localIO $ putStr "connecting to: " >> print remotenode

    newNodes <- runAt remotenode $ interchange  nodes

    local $ return ()                                                              !> "interchange finish"

    -- add the new  nodes to the local nodes in all the nodes connected previously

    let toAdd=remotenode:tail newNodes
    callNodes' nodes  (<>) mempty $ local $ do
           liftIO $ putStr  "New nodes: " >> print toAdd !> "NEWNODES"
           addNodes toAdd

    where
    -- receive new nodes and send their own
    interchange  nodes=
        do
           newNodes <- local $ do
              conn@Connection{remoteNode=rnode, connData=Just cdata} <- getSData <|>
               error ("connect': need to be connected to a node: use wormhole/connect/listen")


              -- if is a websockets node, add only this node
              -- let newNodes = case  cdata of
              --                  Node2Web _ -> [(head nodes){nodeServices=[("relay",show remotenode)]}]
              --                  _ ->  nodes
              
              let newNodes=  nodes -- map (\n -> n{nodeServices= nodeServices n ++ [("relay",show (remotenode,n))]}) nodes

              callingNode<- fixNode $ head newNodes

              liftIO $ writeIORef rnode $ Just callingNode

              liftIO $ modifyMVar_ (fromJust $ connection callingNode) $ const $ return [conn]


              -- onException $ \(e :: SomeException) -> do
              --      liftIO $ putStr "connect:" >> print e
              --      liftIO $ putStrLn "removing node: " >> print callingNode
              --     --  topState >>= showThreads
              --      nodes <- getNodes
              --      setNodes $ nodes \\ [callingNode]

              return newNodes

           oldNodes <- local $ getNodes


           mclustered . local $ do
                liftIO $ putStrLn  "New nodes: " >> print newNodes

                addNodes newNodes  

           localIO $ atomically $ do
                  -- set the first node (local node) as is called from outside
--                     return () !> "HOST2 set"
                     nodes <- readTVar  nodeList
                     let nodes'= (head nodes){nodeHost=nodeHost remotenode
                                             ,nodePort=nodePort remotenode}:tail nodes
                     writeTVar nodeList nodes'


           return oldNodes

#else
connect _ _= empty
connect' _ = empty
#endif

#ifndef ghcjs_HOST_OS
-------------------------------  HTTP client ---------------

class Typeable a => Loggable1 a where
   serialize :: a -> BS.ByteString
   deserialize :: BS.ByteString -> Maybe (a, BS.ByteString)


instance {-# Overlapping #-}  Loggable1 Value where
   serialize= encode
   deserialize msg= case decode msg !> "DECODE" of
          Just x -> Just (x,mempty)
          _      -> Nothing


instance{-# Overlappable #-} (Typeable a, Show a, Read a) => Loggable1 a where
   serialize = BS.pack . show
   deserialize s= case readsPrec 0 (BS.unpack  s)!> "READS" of
        [] -> Nothing
        (r,t): _ -> Just (r,BS.pack t)


newtype HTTPHeaders= HTTPHeaders  [(CI BC.ByteString,BC.ByteString)] deriving Show

rawREST :: Loggable1 a => Node -> String -> TransIO  a
rawREST node restmsg = do
  sock <- liftIO $ connectTo' 8192 (nodeHost node) (  PortNumber $ fromIntegral $ nodePort node) 
  liftIO $ SBS.sendMany sock $ BL.toChunks $ BS.pack $ restmsg 
  return () !> "after send"
  str  <- liftIO $ SBSL.getContents sock

  setParseString str

  headers <- getHeaders
  setState $ HTTPHeaders headers
  case lookup "Transfer-Encoding" headers of
    Just "chunked" -> (dechunk |- ((many $ decodeIt) >>= choose))
    
    _ ->  
         case fmap (read . BC.unpack) $ lookup "Content-Length" headers  of

           Just length -> do
                 msg <- tTake length 
                 return $ deserialize' msg 
           _ -> decodeIt
  where
  deserialize' msg= case deserialize msg !> ("msg",msg)  of
                     Nothing -> error "callRestService : type mismatch" 
                     Just (x,_)  ->  x
  jsonDelimit= return "{" <> (braces $ chainMany mappend1 (dropSpaces >> (jsonDelimit <|> elemString))) <> return "}"
  elemString= (dropSpaces >> tTakeWhile (\c -> c /= '}' && not ( isSpace c)))
  mappend1 a b= a <> " " <> b
 
 
  decodeIt= jsonDelimit >>= return . deserialize'
  
hex= withData $ \s -> if BS.null s then empty else
                        let h= BS.head s 
                            t= BS.tail s
                        in if h >= '0' && h <= '9' then return(ord(h) -ord '0',t)
                           else if h >= 'A' && h <= 'F' then return(ord h -ord 'A' +10,t)
                           else empty
            
numChars= do l <- hex ; tDrop 2 >> return l !> l

dechunk= do
       n<- numChars 
       r <- tTake $ fromIntegral n
       return () !> "drop"
       tDropUntilToken "\r\n"
       return () !> "SMORE"
       return $ SMore r  
       
   <|> return SDone

#endif
 
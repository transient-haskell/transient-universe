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


{-# LANGUAGE DeriveDataTypeable , ExistentialQuantification, OverloadedStrings
    ,ScopedTypeVariables, StandaloneDeriving, RecordWildCards, FlexibleContexts, CPP
    ,GeneralizedNewtypeDeriving #-}
module Transient.Move.Internals where

import Transient.Internals
import Transient.Logged
import Transient.Indeterminism
--  import Transient.Backtrack
import Transient.EVars


import Data.Typeable
import Control.Applicative
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
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Internal          as BLC
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.ByteString.Lazy.Char8             as BS
import           Network.Socket.ByteString              as SBS(sendMany,sendAll,recv)
import qualified Network.Socket.ByteString.Lazy         as SBSL
import           Data.CaseInsensitive(mk)
import           Data.Char(isSpace)

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
-- import System.IO
import Control.Exception hiding (onException,try)
import Data.Maybe
--import Data.Hashable

--import System.Directory
-- import Control.Monad

import System.IO.Unsafe
import Control.Concurrent.STM as STM
import Control.Concurrent.MVar

import Data.Monoid
import qualified Data.Map as M
import Data.List (nub,(\\)) -- ,find, insert)
import Data.IORef



-- import System.IO

import Control.Concurrent



-- import Data.Dynamic
import Data.String

import System.Mem.StableName
import Unsafe.Coerce



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
newtype Cloud a= Cloud {runCloud' ::TransIO a} deriving (Functor,Applicative,Monoid,Alternative, Monad, Num, MonadState EventF)


-- | Execute a distributed computation inside a TransIO computation.
-- All the  computations in the TransIO monad that enclose the cloud computation must be `logged`
runCloud :: Cloud a -> TransIO a
runCloud x= do
       closRemote  <- getSData <|> return (Closure 0)
       runCloud' x <*** setData  closRemote


--instance Monoid a => Monoid (Cloud a) where
--   mappend x y = mappend <$> x <*> y
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
-- | run the cloud computation.
runCloudIO :: Typeable a =>  Cloud a -> IO (Maybe a)
runCloudIO (Cloud mx)= keep mx

-- | run the cloud computation with no console input
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
-- Here foo will be executed in node' but foo' bar and baz don't.
--
-- However foo bar and baz will e executed in node.
--

onAll ::  TransIO a -> Cloud a
onAll =  Cloud

-- | only executes if the result is demanded
lazy :: TransIO a -> Cloud a
lazy mx= onAll $ getCont >>= \st -> Transient $
        return $ unsafePerformIO $  runStateT (runTrans mx) st >>=  return .fst

-- log the result a cloud computation. like `loogged`, this erases all the log produced by computations
-- inside and substitute it for that single result when the computation is completed.
loggedc :: Loggable a => Cloud a -> Cloud a
loggedc (Cloud mx)= Cloud $ do
    closRemote  <- getSData <|> return (Closure 0 )
    logged mx <*** setData  closRemote


loggedc' :: Loggable a => Cloud a -> Cloud a
loggedc' (Cloud mx)= Cloud $ logged mx

-- | the `Cloud` monad has no `MonadIO` instance. `lliftIO= local . liftIO`
lliftIO :: Loggable a => IO a -> Cloud a
lliftIO= local . liftIO

-- | locally perform IO. `localIO = lliftIO`
localIO :: Loggable a => IO a -> Cloud a
localIO= lliftIO

-- | stop the current computation and does not execute any alternative computation
fullStop :: TransIO stop
fullStop= setData WasRemote >> stop


-- | continue the execution in a new node
-- all the previous actions from `listen` to this statement must have been logged
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
  --  wormhole node $  do
  --      relay <- local $ do
  --             conn <-  getState <|> error ("no connection with node: " ++ show node)
  --             case connData conn of  
  --               Just (Relay conn remoteNode) -> do
  --                  setData conn   !> "callTo RELAY"
  --                  return $ Just remoteNode 
  --               _ -> return Nothing
  --      case relay of
  --          Just remoteNode  ->
  --             atRemote $ callTo remoteNode remoteProc
  --          _ ->
  --             atRemote remoteProc  !> "callTo NO RELAY"

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

-- |  Within an open connection to other node opened by `wormhole`, it run the computation in the remote node and return
-- the result back  to the original node.
atRemote :: Loggable a => Cloud a -> Cloud a
atRemote proc= loggedc' $ do
     teleport                                              -- !> "teleport 1111"
     r <- Cloud $ runCloud proc <** setData WasRemote
     teleport                                              -- !> "teleport 2222"
     return r

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
-- if foo  return differnt mainbox indentifiers, the above code would print the
-- messages of  the last one.
-- Without single, it would print the messages of all of them.
single :: TransIO a -> TransIO a
single f= do
   cutExceptions
   Connection{closChildren=rmap} <- getSData <|> error "single: only works within a wormhole"
   mapth <- liftIO $ readIORef rmap
   id <- liftIO $ f `seq` makeStableName f >>= return .  hashStableName


   case  M.lookup id mapth of
          Just tv -> liftIO $ killBranch'  tv      -- !> "JUSTTTTTTTTT"
          Nothing ->  return ()           -- !> "NOTHING"


   tv <- get
   f <** do
          id <- liftIO $ makeStableName f >>= return . hashStableName
          liftIO $ modifyIORef rmap $ \mapth -> M.insert id tv mapth


-- | run an unique continuation for each connection. The first thread that execute `unique` is
-- executed for that connection. The rest are ignored.
unique :: a -> TransIO ()
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




-- | A wormhole opens a connection with another node anywhere in a computation.
-- `teleport` uses this connection to translate the computation back and forth between the two nodes connected
wormhole :: Loggable a => Node -> Cloud a -> Cloud a
wormhole node (Cloud comp) = local $ Transient $ do
   moldconn <- getData :: StateIO (Maybe Connection)
   mclosure <- getData :: StateIO (Maybe Closure)
   labelState $ "wormhole" ++ show node
   Log rec _ _ <- getData `onNothing` return (Log False [][])


   if not rec                                    
            then runTrans $ (do

                    conn <-  mconnect node
                    liftIO $ writeIORef (remoteNode conn) $ Just node
                    setData  conn{calling= True}

                    comp )
                  <*** do when (isJust moldconn) . setData $ fromJust moldconn
                          when (isJust mclosure) . setData $ fromJust mclosure
                    -- <** is not enough since comp may be reactive
            else do
             let conn = fromMaybe (error "wormhole: no connection in remote node") moldconn

             setData $ conn{calling= False}

             runTrans $  comp
                     <*** do

                          when (isJust mclosure) . setData $ fromJust mclosure




#ifndef ghcjs_HOST_OS
type JSString= String
pack= id



#endif


teleport ::   Cloud ()
teleport =  do
  local $ Transient $ do
     cont <- get
     labelState "teleport"
     -- send log with closure at head
     Log rec log fulLog <- getData `onNothing` return (Log False [][])
     if not rec   -- !> ("teleport rec,loc fulLog=",rec,log,fulLog)
                  -- if is not recovering in the remote node then it is active
      then  do
        conn@Connection{connData=contype,closures= closures,calling= calling} <- getData
             `onNothing` error "teleport: No connection defined: use wormhole"
#ifndef ghcjs_HOST_OS
        case contype of
         Just Self ->  runTrans $ do
--               return () !> "SELF"
               setData $  if (not calling) then  WasRemote else WasParallel
               abduce  -- !> "SELF" -- call himself
               liftIO $ do
                  remote <- readIORef $ remoteNode conn
                  writeIORef (myNode conn) $ fromMaybe (error "teleport: no connection?") remote


         _ -> do
#else
        do
#endif

         --read this Closure
          Closure closRemote  <- getData `onNothing` return (Closure 0 )

         --set his own closure in his Node data

          let closLocal = sum $ map (\x-> case x of Wait -> 100000;
                                                    Exec -> 1000
                                                    _ -> 1) fulLog

--         closLocal  <-   liftIO $ randomRIO (0,1000000)
--          node <- runTrans getMyNode
          
          liftIO $ modifyMVar_ closures $ \map -> return $ M.insert closLocal (fulLog,cont) map

          let tosend= reverse $ if closRemote==0 then fulLog else  log
          
          runTrans $ do msend conn $ SMore $ ClosureData closRemote closLocal tosend
                                     !> ("teleport sending", SMore (closRemote,closLocal,tosend))
                                     !> "--------->------>---------->"

          setData $ if (not calling) then  WasRemote else WasParallel

          return Nothing

      else do

         delData WasRemote                -- !> "deleting wasremote in teleport"
                                          -- it is recovering, therefore it will be the
                                          -- local, not remote

         return (Just ())                           --  !> "TELEPORT remote"




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
putMailbox :: Typeable a => a -> TransIO ()
putMailbox = putMailbox' (0::Int)

-- | write to a mailbox identified by an Integer besides the type
putMailbox' :: (Typeable b, Ord b, Typeable a) =>  b -> a -> TransIO ()
putMailbox'  idbox dat= do
   let name= MailboxId idbox $ typeOf dat
   Connection{comEvent= mv} <- getData `onNothing` errorMailBox
   mbs <- liftIO $ readIORef mv
   let mev =  M.lookup name mbs
   case mev of
     Nothing ->newMailbox name >> putMailbox' idbox dat
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
getMailbox :: Typeable a => TransIO a
getMailbox = getMailbox' (0 :: Int)

-- | read from a mailbox identified by a number besides the type
getMailbox' :: (Typeable b, Ord b, Typeable a) => b -> TransIO a
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
cleanMailbox = cleanMailbox' 0

-- | clean a mailbox identified by an Int and the type
cleanMailbox' :: Typeable a => Int ->  a -> TransIO ()
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
sendRaw (Connection _ _ (Just (Node2Web  sconn )) _ _ _ _ _ _) r=
      liftIO $   WS.sendTextData sconn  r                                --  !> ("NOde2Web",r)

sendRaw (Connection _ _ (Just (Node2Node _ sock _)) _ _ blocked _ _ _) r=
      liftIO $  withMVar blocked $ const $  SBS.sendMany sock
                                      (BL.toChunks r )                   -- !> ("NOde2Node",r)

sendRaw (Connection _ _(Just (TLSNode2Node  ctx )) _ _ blocked _ _ _) r=
      liftIO $ withMVar blocked $ const $ sendTLSData ctx  r       --  !> ("TLNode2Web",r)

#else
sendRaw (Connection _ _ (Just (Web2Node sconn)) _ _ blocked _  _ _) r= liftIO $
   withMVar blocked $ const $ JavaScript.Web.WebSocket.send   r sconn   -- !!> "MSEND SOCKET"
#endif

sendRaw _ _= error "No connection stablished"


data NodeMSG= ClosureData IdClosure IdClosure CurrentPointer    | RelayMSG Node Node (StreamData NodeMSG) 
   deriving (Typeable, Read, Show)

msend :: MonadIO m => Connection -> StreamData NodeMSG -> m ()

#ifndef ghcjs_HOST_OS

msend (Connection _ _ (Just (Node2Node _ sock _)) _ _ blocked _ _ _) r=do
   liftIO $   withMVar blocked $  const $ SBS.sendAll sock $ BC.pack (show r)   !> "N2N SEND"

msend (Connection _ _ (Just (TLSNode2Node ctx)) _ _ _ _ _ _) r=
     liftIO $ sendTLSData  ctx $ BS.pack (show r)                             -- !> "TLS SEND"


msend (Connection _ _ (Just (Node2Web sconn)) _ _ _ _  _ _) r=liftIO $
  {-withMVar blocked $ const $ -} WS.sendTextData sconn $ BS.pack (show r)   -- !> "websockets send"

msend((Connection myNode _ (Just (Relay conn remote )) _ _ _ _  _ _)) r= do
   origin <- liftIO $ readIORef myNode  -- `onNothing` error "msend: no remote node in connection"
  --  msend conn $ SMore (ClosureData 0 0 [Var $ IDynamic (),Var . IDynamic $ origin{nodeServices=[]}
  --                                     ,Var $ IDynamic remote{nodeServices=[]},Var $ IDynamic r])  -- writeEVar req r  !> "msed relay" 
   msend conn $ SMore $ RelayMSG origin remote r


#else

msend (Connection _ _ (Just (Web2Node sconn)) _ _ blocked _  _ _) r= liftIO $
  withMVar blocked $ const $ JavaScript.Web.WebSocket.send  (JS.pack $ show r) sconn   -- !!> "MSEND SOCKET"



#endif

msend (Connection _ _ Nothing _ _  _ _ _ _) _= error "msend out of wormhole context"

mread :: Loggable a => Connection -> TransIO (StreamData a)


#ifdef ghcjs_HOST_OS


mread (Connection _ _ (Just (Web2Node sconn)) _ _ _ _  _ _)=  wsRead sconn



wsRead :: Loggable a => WebSocket  -> TransIO  a
wsRead ws= do
  dat <- react (hsonmessage ws) (return ())
  case JM.getData dat of
    JM.StringData str  ->  return (read' $ JS.unpack str)
        --         !> ("Browser webSocket read", str)  !> "<------<----<----<------"
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
  cb <- makeCallback MessageEvent hscb
  js_onmessage ws cb

foreign import javascript safe
             "$1.onopen =$2;"
   js_open :: WebSocket  -> JSVal  -> IO ()

newtype OpenEvent = OpenEvent JSVal deriving Typeable
hsopen ::  WebSocket -> (OpenEvent ->IO()) -> IO ()
hsopen ws hscb= do
   cb <- makeCallback OpenEvent hscb
   js_open ws cb

makeCallback :: (JSVal -> a) ->  (a -> IO ()) -> IO JSVal

makeCallback f g = do
   Callback cb <- CB.syncCallback1 CB.ContinueAsync (g . f)
   return cb


foreign import javascript safe
   "new WebSocket($1)" js_createDefault :: JS.JSString -> IO WebSocket


#else
mread (Connection _ _(Just (Node2Node _ _ _)) _ _ _ _ _ _) =  parallelReadHandler -- !> "mread"

mread (Connection _ _ (Just (TLSNode2Node _)) _ _ _ _ _ _) =  parallelReadHandler
--        parallel $ do
--            s <- recvTLSData  ctx
--            return . read' $  BC.unpack s

mread (Connection _ _  (Just (Node2Web sconn )) _ _ _ _ _ _)=
        parallel $ do
            s <- WS.receiveData sconn
            return . read' $  BS.unpack s
              --  !>  ("WS MREAD RECEIVED ----<----<------<--------", s)

mread (Connection  _ _ (Just (Relay conn _  )) _ _ _ _ _ _)=  
     mread conn  -- !> "MREAD RELAY"
       



parallelReadHandler :: Loggable a => TransIO (StreamData a)
parallelReadHandler= do
      str <- giveData :: TransIO BS.ByteString
      r <- choose $ readStream str

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

read' s= case readsPrec' 0 s of
       [(x,"")] -> x
       _  -> error $ "reading " ++ s

--release (Node h p rpool _) hand= liftIO $ do
----    print "RELEASED"
--    atomicModifyIORef rpool $  \ hs -> (hand:hs,())
--      -- !!> "RELEASED"

mclose :: Connection -> IO ()

#ifndef ghcjs_HOST_OS

mclose (Connection _ _
   (Just (Node2Node _  sock _ )) _ _ _ _ _ _)= NS.close sock

mclose (Connection _ _
   (Just (Node2Web sconn ))
   _ _ _ _  _ _)=
    WS.sendClose sconn ("closemsg" :: BS.ByteString)

#else

mclose (Connection _ _(Just (Web2Node sconn)) _ _ blocked _ _ _)=
    JavaScript.Web.WebSocket.close Nothing Nothing sconn

#endif



mconnect :: Node -> TransIO  Connection
mconnect  node'=  do
  node <- fixNode node'
  nodes <- getNodes
  return ()                                            --    !>  ("mconnnect", nodePort node)
  let fnode =  filter (==node) nodes
  case fnode of
   [] -> mconnect1 node   -- !> "NO NODE"
   [node'@(Node _ _ pool _)] -> do
      plist <- liftIO $  readMVar $ fromJust pool 
      case plist of                                      --   !>  ("length", length plist,nodePort node) of
        (handle:_) -> do
                  delData $ Closure undefined
                  return  handle
--                                                            !>   ("REUSED!", node)

        _ -> mconnect1 node'                                 
  where


#ifndef ghcjs_HOST_OS
  mconnect1 (node@(Node host port _ _))= do

     return ()  !> ("MCONNECT1",host,port,nodeServices node)
--     (do
--      liftIO $ when (host== "192.168.99.100" && (port == 8081 || port== 8080)) $  error "connnnn"  !> "detected"
     (conn,parseContext) <- timeout 1000000 (connectNode2Node host port)   <|>
                            timeout 1000000 (connectWebSockets host port)  <|> 
                            checkRelay                                     <|>
                            (throwt $ ConnectionError "" node)

     setState conn
     setState parseContext
--     return () !> "CONNECTED AFTER TIMEOUT"

     -- write node connected in the connection
     liftIO $ writeIORef (remoteNode conn) $ Just node
     -- write connection in the node
     liftIO $ modifyMVar_ (fromJust $ connection node) . const $ return [conn]
     addNodes [node]

     watchConnection
     delData $ Closure undefined
     return  conn

    where

    timeout t proc=do
       r <- collect' 1 t proc
       case r of
          []  -> empty
          r:_ -> return r

    checkRelay= do
        return () !> "RELAY"
        myNode <- getMyNode
        if nodeHost node== nodeHost myNode
                then
                    case lookup "localNode" $ nodeServices node   of
                            Just snode -> do
                                con <- mconnect $ read snode 
                                cont <- getSData <|> return  noParseContext
                                return (con,cont)
                            Nothing -> empty
                else do

                  case lookup "relay" $ nodeServices node of
                    Nothing -> empty  -- !> "NO RELAY"
                    Just relayInfo -> do
                      let relay= read relayInfo
                      conn <- mconnect relay        -- !> ("RELAY",relay, node)
                      rem <- liftIO $ newIORef $ Just node
                      -- clos <- liftIO $ newMVar $ M.empty
                      let conn'= conn{connData= Just $ Relay conn node,remoteNode=rem} --,closures= clos}
                    
                      parseContext <- getState <|> return noParseContext
                      return (conn', parseContext)

    noParseContext= (ParseContext (error "relay error") (error "relay error")
                             ::  ParseContext BS.ByteString)

    connectSockTLS host port= do
        -- return ()                                         !> "connectSockTLS"

        let size=8192
        Connection{myNode=my,comEvent= ev} <- getSData <|> error "connect: listen not set for this node"

        sock  <- liftIO $ connectTo'  size  host $ PortNumber $ fromIntegral port

        conn' <- liftIO $ defConnection >>= \c ->
                     return c{myNode=my, comEvent= ev,connData=
                     
                     Just $ (Node2Node u  sock (error $ "addr: outgoing connection"))}

        setData conn'
        input <-  liftIO $ SBSL.getContents sock

        setData $ ParseContext (error "parse context: Parse error") input

        
        maybeClientTLSHandshake host sock input


      `catcht` \(_ :: SomeException) ->   empty 


    connectNode2Node host port= do
        return () !> "NODE 2 NODE"
        connectSockTLS host port

        conn <- getSData <|> error "mconnect: no connection data"
        sendRaw conn "CLOS a b\r\n\r\n"
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


    connectWebSockets host port = do
         return () !> "WEBSOCKETS"
         connectSockTLS host port  -- a new connection

         never  <- liftIO $ newEmptyMVar :: TransIO (MVar ())
         conn   <- getSData  <|> error "connectWebSockets: no connection"
         stream <- liftIO $ makeWSStreamFromConn conn
         wscon  <- react (NWS.runClientWithStream stream (host++(':': show port)) "/"
                      WS.defaultConnectionOptions []) (takeMVar never)


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
--    my <- getMyNode

    Connection{myNode=my,comEvent= ev} <- getSData <|> error "connect: listen not set for this node"

    do
        ws <- connectToWS host $ PortNumber $ fromIntegral port
--                                                           !> "CONNECTWS"
        conn <- defConnection >>= \c -> return c{comEvent= ev,connData= Just  (Web2Node ws)}
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
   let ps = case (protocol :: JSString)of "http:" -> "ws://"; "https:" -> "wss://"
   wsOpen $ JS.pack $ ps++ h++ ":"++ show p
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
                   | Relay Connection Node  -- (EVar (StreamData NodeMSG))
#else
                   Self
                   | Web2Node{webSocket :: WebSocket}
#endif



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
         GT -> GT
         LT -> LT

data Connection= Connection{myNode     :: IORef Node
                           ,remoteNode :: IORef (Maybe Node)
                           ,connData   :: Maybe ConnectionData
                           ,bufferSize :: BuffSize
                           -- Used by getMailBox, putMailBox
                           ,comEvent   :: IORef (M.Map MailboxId (EVar SData))
                           -- multiple wormhole/teleport use the same connection concurrently
                           ,blocked    :: Blocked
                           ,calling    :: Bool
                           -- local closures with his log and his continuation
                           ,closures   :: MVar (M.Map IdClosure ([LogElem], EventF))

                           -- for each remote closure that points to local closure 0,
                           -- a new container of child processes
                           -- in order to treat them separately
                           -- so that 'killChilds' do not kill unrelated processes
                           ,closChildren :: IORef (M.Map Int EventF)}

                  deriving Typeable









defConnection :: MonadIO m => m Connection

-- #ifndef ghcjs_HOST_OS
defConnection = liftIO $ do
  my <- newIORef (error "node in default connection")
  x <- newMVar ()
  y <- newMVar M.empty
  noremote <- newIORef Nothing
  z <-  return $ error "closchildren newIORef M.empty"
  return $ Connection my noremote Nothing  8192
                 (error "defConnection: accessing network events out of listen")
                 x  False y z



#ifndef ghcjs_HOST_OS
setBuffSize :: Int -> TransIO ()
setBuffSize size= Transient $ do
   conn<- getData `onNothing`  defConnection
   setData $ conn{bufferSize= size}
   return $ Just ()

getBuffSize=
  (do getSData >>= return . bufferSize) <|> return  8192




listen ::  Node ->  Cloud ()
listen  (node@(Node _   port _ _ )) = onAll $ do
   addThreads 1
   setData $ Log False [] []

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
   mlog <- listenNew (fromIntegral port) conn  <|> listenResponses :: TransIO (StreamData NodeMSG)
   case mlog of 
       SMore (RelayMSG _ _ _) ->relay mlog
       _ -> execLog  mlog
 `catcht` (\(e ::SomeException) -> liftIO $ print  e)


-- relayService :: TransIO ()

relay (SMore (RelayMSG origin destiny streamdata)) = do
  nodes <- getNodes
  my <- getMyNode                        -- !> "relayService"
  if destiny== my 
    then do
       case  filter (==origin) nodes of 
          [node] -> do
              (conn: _) <- liftIO $ readMVar $ fromJust $ connection node
              setData  conn
              
          [] -> do
              conn@Connection{remoteNode= rorigin} <- getState 
              let conn'= conn{connData= Just $ Relay conn origin}       --  !> ("Relay set with: ",  origin, destiny)
              pool <- liftIO $ newMVar [conn']
              addNodes [origin{connection= Just pool}]
              setData conn'
       execLog streamdata 
       
    else do
          -- search local node name if hostname is the same 

          -- let destiny' = if nodeHost destiny== nodeHost my 
          --       then
          --           case filter (==destiny) nodes  of 
          --               [node]  -> case lookup "localNode" $ nodeServices node   of
          --                   Just snode ->  read snode 
          --                   Nothing -> destiny
          --               _ -> destiny
          --       else destiny
          -- let origin'=  if nodeHost origin == "localhost" 
          --          then case filter (==origin) nodes of 
          --               [node]  ->case lookup "externalNode" $ nodeServices node of
          --                            Just snode -> read snode
          --                            Nothing -> origin 
          --               _ -> origin
          --       else origin 

          let (origin',destiny')= nat  origin destiny  my nodes
          con <- mconnect destiny'
          msend con . SMore $ RelayMSG origin' destiny' streamdata
          return () !> ("SEND RELAY DATA",streamdata)
          fullStop
  
 
relay _= empty

nat  origin destiny  my nodes= 
          let destiny' = if nodeHost destiny== nodeHost my 
                then
                    case filter (==destiny) nodes  of 
                        [node]  -> case lookup "localNode" $ nodeServices node   of
                            Just snode ->  read snode 
                            Nothing -> destiny
                        _ -> destiny
                else destiny
              origin'=  if nodeHost origin == "localhost" 
                   then case filter (==origin) nodes of 
                        [node]  ->case lookup "externalNode" $ nodeServices node of
                                     Just snode -> read snode
                                     Nothing -> origin 
                        _ -> origin
                else origin 
          in (origin',destiny')

-- listen incoming requests
listenNew port conn'= do

   sock <- liftIO . listenOn $ PortNumber port

   let bufSize= bufferSize conn'
   liftIO $ do NS.setSocketOption sock NS.RecvBuffer bufSize
               NS.setSocketOption sock NS.SendBuffer bufSize

   -- wait for connections. One thread per connection
   (sock,addr) <- waitEvents $ NS.accept sock
   chs <- liftIO $ newIORef M.empty
--   case addr of
--     NS.SockAddrInet port host -> liftIO $ print("connection from", port, host)
--     NS.SockAddrInet6  a b c d -> liftIO $ print("connection from", a, b,c,d)
   noNode <- liftIO $ newIORef Nothing
   let conn= conn'{closChildren=chs, remoteNode= noNode}

   input <-  liftIO $ SBSL.getContents sock

   cutExceptions
   
   onException $ \(e :: SomeException) -> do
            --  cutExceptions
             liftIO $ putStr "listen: " >> print e
             
             let Connection{remoteNode=rnode,closures=closures,closChildren= rmap} = conn
             -- TODO How to close Connection by discriminating exceptions
             mnode <- liftIO $ readIORef rnode
             case mnode of
               Nothing -> return ()
               Just node  -> do
                             liftIO $ putStr "removing1 node: " >> print node
                             nodes <- getNodes
                             setNodes $ nodes \\ [node]
             liftIO $ do
                  modifyMVar_ closures $ const $ return M.empty
                  writeIORef rmap M.empty
            --  topState >>= showThreads
            -- cutExceptions
             killBranch
             

   setData $ (ParseContext (NS.close sock >> error "Communication error" ) input
             ::ParseContext BS.ByteString)

   setState conn{connData=Just (Node2Node (PortNumber port) sock addr)}
   maybeTLSServerHandshake sock input



   (method,uri, headers) <- receiveHTTPHead
   maybeSetHost headers
   case method of

     "CLOS" ->
          do
           conn <- getSData
           sendRaw conn "OK"                               --      !> "CLOS detected"

           mread conn

     _ -> do
           let uri'= BC.tail $ uriPath uri
           if  "api/" `BC.isPrefixOf` uri'
             then do


               log <- return $ Exec: (Var $ IDyns $ BS.unpack method):(map (Var . IDyns ) $ split $ BC.unpack $ BC.drop 4 uri')


               str <-  giveData  <|> error "no api data"

               log' <- case (method,lookup "Content-Type" headers) of
                       ("POST",Just "application/x-www-form-urlencoded") -> do
                            len <- read <$>  BC.unpack
                                        <$> (Transient $ return (lookup "Content-Length" headers))
                            setData $ ParseContext (return mempty) $ BS.take len str

                            postParams <- parsePostUrlEncoded  <|> return []
                            return $ log ++  [(Var . IDynamic $ postParams)]

                       _ -> return $ log  -- ++ [Var $ IDynamic  str]

               return $ SMore $ ClosureData 0 0 log'

             else do

                   -- stay serving pages until a websocket request is received
                   servePages (method, uri', headers)
                   conn <- getSData
                   sconn <- makeWebsocketConnection conn uri headers
                   -- websockets mode


                   let conn'= conn{connData= Just (Node2Web sconn)
                             , closChildren=chs}
                   setState conn'   -- !> "WEBSOCKETS-----------------------------------------------"
                   onException $ \(e :: SomeException) -> do
                            cutExceptions
                            liftIO $ putStr "listen websocket:" >> print e
                            --liftIO $ mclose conn'
                            killBranch
                            empty
--                   async (return (SMore (0,0,[Exec]))) <|> do
                   do
--                     return ()                                                   !> "WEBSOCKET"
                     r <-  parallel $ do
                             msg <- WS.receiveData sconn
                            --  return () !> ("Server WebSocket msg read",msg)
                            --            !> "<-------<---------<--------------"

                             case reads $ BS.unpack msg of
                               [] -> do
                                   let log =Exec: [Var $ IDynamic  (msg :: BS.ByteString)]
                                   return $ SMore (ClosureData 0 0 log)
                               ((x ,_):_) -> return (x :: StreamData NodeMSG) -- StreamData (Int,Int,[LogElem]))

                     case r of
                       SError e -> do
--                           liftIO $ WS.sendClose sconn ("error" :: BS.ByteString)
                           back e
--                                                                 !> "FINISH1"
                       _ -> return r

     where
      uriPath = BC.dropWhile (/= '/')
      split []= []
      split ('/':r)= split r
      split s=
          let (h,t) = span (/= '/') s
          in h: split  t


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

listenResponses :: Loggable a => TransIO (StreamData a)
listenResponses= do
      (conn, parsecontext, node) <- getMailbox

      labelState $ "listen from: "++ show node
--      return () !> ("LISTEN",case connData conn of Just (Relay _) -> "RELAY"; _ -> "OTHER")
      setData conn

#ifndef ghcjs_HOST_OS
      setData (parsecontext :: ParseContext BS.ByteString)
#else
      setData (parsecontext :: ParseContext JSString)
#endif



      cutExceptions
      onException (\(e:: SomeException) -> do
                             liftIO $ putStr "ListenResponses: " >> print e
                             liftIO $ putStr "removing node: " >> print node
                             nodes <- getNodes
                             setNodes $ nodes \\ [node]
                            --  topState >>= showThreads
                             killChilds
                             let Connection{closures=closures}= conn
                             liftIO $ modifyMVar_ closures $ const $ return M.empty)


      mread conn




type IdClosure= Int

newtype Closure= Closure IdClosure deriving Show

execLog :: StreamData NodeMSG -> TransIO ()
execLog  mlog = Transient $
       case  mlog   of
             SError e -> do
                 runTrans $ back e
                 return Nothing

             SDone   -> runTrans(back $ ErrorCall "SDone") >> return Nothing   -- TODO remove closure?
             SMore r -> process r False
             SLast r -> process r True
  -- !> ("EXECLOG",mlog)
   where

   process (ClosureData closl closr log) deleteClosure= do
      conn@Connection {closures=closures} <- getData `onNothing` error "Listen: myNode not set"
      if closl== 0 then do
           setData $ Log True log  $ reverse log
           setData $ Closure closr
           return $ Just ()                  --  !> "executing top level closure"
       else do

         mcont <- liftIO $ modifyMVar closures
                         $ \map -> return (if deleteClosure then
                                           M.delete closl map
                                         else map, M.lookup closl map)
                                           -- !> ("closures=", M.size map)
         case mcont of
           Nothing -> do
--
--              if closl == 0   -- add what is after execLog as closure 0
--               then do
--                     setData $ Log True log  $ reverse log
--                     setData $ Closure closr
--                     cont <- get    !> ("CLOSL","000000000")
--                     liftIO $ modifyMVar closures
--                            $ \map -> return (M.insert closl ([],cont) map,())
--                     return $ Just ()     --exec what is after execLog (closure 0)
--
--               else do
                     runTrans $ msend conn $ SLast (ClosureData closr closl [])
                        -- to delete the remote closure
                     runTrans $ liftIO $ error ("request received for non existent closure: "
                                             ++  show closl)
           -- execute the closure
           Just (fulLog,cont) -> do
                        liftIO $ runStateT (do
                             let nlog= reverse log ++  fulLog
                             setData $ Log True  log  nlog
                             setData $ Closure closr
--                                                                !> ("SETCLOSURE",closr)
                             runContinuation cont ()) cont
                        return Nothing


#ifdef ghcjs_HOST_OS
listen node = onAll $ do
        addNodes [node]

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
data ParseContext a = IsString a => ParseContext (IO  a) a deriving Typeable


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
        mcontent <- liftIO $ (Just <$> BL.readFile ( "./static/out.jsexe/"++ BC.unpack file))
                                `catch` (\(e:: SomeException) -> return Nothing)
--                                    return  "Not found file: index.html<br/> please compile with ghcjs<br/> ghcjs program.hs -o static/out")

        case mcontent of
          Just content -> liftIO $ sendRaw conn $
            "HTTP/1.0 200 OK\r\nContent-Type: text/html\r\nConnection: close\r\nContent-Length: "
            <> BS.pack (show $ BL.length content) <>"\r\n\r\n" <> content

          Nothing ->liftIO $ sendRaw conn $ BS.pack $ "HTTP/1.0 404 Not Found\nContent-Length: 0\nConnection: close\n\n"
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

receiveHTTPHead = do

  (method, uri, vers) <- (,,) <$> getMethod <*> getUri <*> getVers
  headers <- manyTill paramPair  (string "\r\n\r\n")          -- !>  (method, uri, vers)
  return (method, toStrict uri, headers)                      -- !>  (method, uri, headers)

  where
  string :: BS.ByteString -> TransIO BS.ByteString
  string s=withData $ \str -> do
      let len= BS.length s
          ret@(s',str') = BS.splitAt len str
      if s == s'
        then return ret
        else empty

  paramPair=  (,) <$> (mk <$> getParam) <*> getParamValue
  manyTill p end  = scan
      where
      scan  = do{ end; return [] }
            <|>
              do{ x <- p; xs <- scan; return (x:xs) }

  getMethod= getString
  getUri= getString
  getVers= getString
  getParam= do
      dropSpaces
      r <- tTakeWhile (\x -> x /= ':' && not (endline x))
      if BS.null r || r=="\r"  then  empty  else  dropChar >> return (toStrict r)

  getParamValue= toStrict <$> ( dropSpaces >> tTakeWhile  (\x -> not (endline x)))

dropSpaces= parse $ \str ->((),BS.dropWhile isSpace str)

dropChar= parse  $ \r -> ((), BS.tail r)

endline c= c== '\n' || c =='\r'

--tGetLine= tTakeWhile . not . endline

type PostParams = [(BS.ByteString, String)]

parsePostUrlEncoded :: TransIO PostParams
parsePostUrlEncoded=  do
   dropSpaces
   many $ (,) <$> param  <*> value
   where
   param= tTakeWhile' ( /= '=')
   value= unEscapeString <$> BS.unpack <$> tTakeWhile' ( /= '&')


getString= do
    dropSpaces
    tTakeWhile (not . isSpace)

tTakeWhile :: (Char -> Bool) -> TransIO BS.ByteString
tTakeWhile cond= parse (BS.span cond)

tTakeWhile' :: (Char -> Bool) -> TransIO BS.ByteString
tTakeWhile' cond= parse ((\(h,t) -> (h, if BS.null t then t else BS.tail t)) . BS.span cond)

parse :: (BS.ByteString -> (b, BS.ByteString)) -> TransIO b
parse split= withData $ \str ->

     if str== mempty   then empty

     else  return $ split str


-- | bring the data of a parse context as a lazy byteString to a parser
-- and actualize the parse context with the result
withData :: (BS.ByteString -> TransIO (a,BS.ByteString)) -> TransIO a
withData parser= Transient $ do
   ParseContext readMore s <- getData `onNothing` error "parser: no context"
   let loop = unsafeInterleaveIO $ do
           r <-  readMore
           (r <>) `liftM` loop
   str <- liftIO $ (s <> ) `liftM` loop
   mr <- runTrans $ parser str
   case mr of
    Nothing -> return Nothing
    Just (v,str') -> do
      setData $ ParseContext readMore str'
      return $ Just v

-- | bring the data of the parse context as a lazy byteString
giveData =noTrans $ do
   ParseContext readMore s <- getData `onNothing` error "parser: no context"
                                  :: StateIO (ParseContext BS.ByteString)  -- change to strict BS

   let loop = unsafeInterleaveIO $ do
           r <-  readMore
           (r <>) `liftM` loop
   liftIO $ (s <> ) `liftM` loop


#endif



#ifdef ghcjs_HOST_OS
isBrowserInstance= True
api _= empty
#else
-- | True if it is running in the browser
isBrowserInstance= False

#endif





{-# NOINLINE emptyPool #-}
emptyPool :: MonadIO m => m (MVar Pool)
emptyPool= liftIO $ newMVar  []


createNodeServ ::  HostName -> Int -> Service -> IO Node
createNodeServ h p svs=  return $ Node h  p Nothing svs


createNode :: HostName -> Int -> IO Node
createNode h p= createNodeServ h p []

createWebNode :: IO Node
createWebNode= do
  pool <- emptyPool
  return $ Node "webnode"  0 (Just pool)  [("webnode","")]


instance Eq Node where
    Node h p _ _ ==Node h' p' _ _= h==h' && p==p'


instance Show Node where
    show (Node h p _ servs )= show (h,p, servs)

instance Read Node where
    readsPrec _ s=
          let r= readsPrec' 0 s
          in case r of
            [] -> []
            [((h,p,ss),s')] ->  [(Node h p Nothing ( ss),s')]
          
          


-- inst    ghc-options: -threaded -rtsopts

nodeList :: TVar  [Node]
nodeList = unsafePerformIO $ newTVarIO []

deriving instance Ord PortID

--myNode :: Int -> DBRef  MyNode
--myNode= getDBRef $ key $ MyNode undefined



errorMyNode f= error $ f ++ ": Node not set. initialize it with connect, listen, initNode..."


getMyNode :: TransIO Node -- (MonadIO m, MonadState EventF m) => m Node
getMyNode =  do
    Connection{myNode= node}  <- getSData   <|> errorMyNode "getMyNode" :: TransIO Connection
    liftIO $ readIORef node



-- | return the list of nodes connected to the local node
getNodes :: MonadIO m => m [Node]
getNodes  = liftIO $ atomically $ readTVar  nodeList

-- getEqualNodes= getNodes 

getEqualNodes = do
    nodes <- getNodes
    let srv= nodeServices $ head nodes
    case srv of
      [] -> return $ filter (null . nodeServices) nodes 
      (srv:_)  -> return $ filter (\n ->  head (nodeServices n) == srv  ) nodes
 
matchNodes f = do
      nodes <- getNodes
      return $ map (\n -> filter f $ nodeServices n) nodes 

-- | add nodes to the list of nodes
addNodes :: [Node] ->  TransIO () -- (MonadIO m, MonadState EventF m) => [Node] -> m ()
addNodes   nodes=  do
--  my <- getMyNode    -- mynode must be first
  nodes' <- mapM fixNode nodes
  liftIO . atomically $ do
    prevnodes <- readTVar nodeList
    writeTVar nodeList $  nub $ prevnodes ++ nodes'

fixNode n= case connection n of
  Nothing -> do
      pool <- emptyPool
      return n{connection= Just pool}
  Just _ -> return n

-- | set the list of nodes
setNodes nodes= liftIO $ atomically $ writeTVar nodeList $  nodes

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

--newtype MyNode= MyNode Node deriving(Read,Show,Typeable)


--instance Indexable MyNode where key (MyNode Node{nodePort=port}) =  "MyNode "++ show port
--
--instance Serializable MyNode where
--    serialize= BS.pack . show
--    deserialize= read . BS.unpack



-- | set the rest of the computation as the code of a new node (first parameter) and connect it
-- to an existing node (second parameter). then it uses `connect`` to synchronize the list of nodes
connect ::  Node ->  Node -> Cloud ()
#ifndef ghcjs_HOST_OS
connect  node  remotenode =   do
    listen node <|> return ()
    connect' remotenode



-- | synchronize the list of nodes with a remote node and all the nodes connected to it
-- the final effect is that all the nodes reachable share the same list of nodes
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
              let newNodes= map (\n -> n{nodeServices= nodeServices n ++ [("relay",show (remotenode,n))]}) nodes

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
                  -- set the firt node (local node) as is called from outside
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






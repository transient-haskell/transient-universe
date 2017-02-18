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
import Transient.Indeterminism(choose)
import Transient.Backtrack
import Transient.EVars


import Data.Typeable
import Control.Applicative
#ifndef ghcjs_HOST_OS
import Network
import Network.Info
import Network.URI
--import qualified Data.IP                              as IP
import qualified Network.Socket                         as NS
import qualified Network.BSD                            as BSD
import qualified Network.WebSockets                     as NWS(RequestHead(..))

import qualified Network.WebSockets.Connection          as WS

import           Network.WebSockets.Stream hiding(parse)
import qualified Data.ByteString                        as B(ByteString,concat)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Internal          as BLC
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.ByteString.Lazy.Char8             as BS
import           Network.Socket.ByteString              as SBS(send,sendMany,sendAll,recv)
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
import System.IO
import Control.Exception hiding (onException,try)
import Data.Maybe
--import Data.Hashable

--import System.Directory
import Control.Monad

import System.IO.Unsafe
import Control.Concurrent.STM as STM
import Control.Concurrent.MVar

import Data.Monoid
import qualified Data.Map as M
import Data.List (nub,(\\),find, insert)
import Data.IORef



import System.IO

import Control.Concurrent



import Data.Dynamic
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
               , connection :: MVar Pool
               , nodeServices   :: [Service]
               }

         deriving (Typeable)

instance Ord Node where
   compare node1 node2= compare (nodeHost node1,nodePort node1)(nodeHost node2,nodePort node2)


-- The cloud monad is a thin layer over Transient in order to make sure that the type system
-- forces the logging of intermediate results
newtype Cloud a= Cloud {runCloud' ::TransIO a} deriving (Functor,Applicative,Monoid,Alternative, Monad, Num, MonadState EventF)


runCloud x= do
       closRemote  <- getSData <|> return (Closure 0)
       runCloud' x <*** setData  closRemote




--instance Monoid a => Monoid (Cloud a) where
--   mappend x y = mappend <$> x <*> y
--   mempty= return mempty

#ifndef ghcjs_HOST_OS

--- empty Hooks for TLS

tlsHooks ::IORef (SData -> BS.ByteString -> IO ()
                 ,SData -> IO B.ByteString
                 ,NS.Socket -> BS.ByteString -> TransIO ()
                 ,String -> NS.Socket -> BS.ByteString -> TransIO ())
tlsHooks= unsafePerformIO $ newIORef (notneeded,notneeded, \s i -> tlsNotSupported i, \_ _ _-> return())
  where notneeded= error "TLS hook function called"

(sendTLSData,recvTLSData,maybeTLSServerHandshake,maybeClientTLSHandshake)= unsafePerformIO $ readIORef tlsHooks

tlsNotSupported input = do
 if ((not $ BL.null input) && BL.head input  == 0x16)
   then  do
     conn <- getSData
     sendRaw conn $ BS.pack $ "HTTP/1.0 525 SSL Handshake Failed\nContent-Length: 0\nConnection: close\n\n"
   else return ()


--sendTLSDatar = unsafePerformIO $ newIORef undefined
--sendTLSData :: SData -> BS.ByteString -> IO ()
--sendTLSData= unsafePerformIO $ readIORef sendTLSDatar
--
--
--recvTLSDatar =  unsafePerformIO $ newIORef undefined
--recvTLSData :: SData -> IO B.ByteString
--recvTLSData = unsafePerformIO $ readIORef recvTLSDatar
--
--maybeTLSServerHandshaker  = unsafePerformIO $ newIORef $ (\_ _ -> (return () :: TransIO ()))

----                                :: NS.Socket -> BS.ByteString -> TransIO ()

--maybeTLSServerHandshake = unsafePerformIO $ readIORef maybeTLSServerHandshaker
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

lazy :: TransIO a -> Cloud a
lazy mx= onAll $ getCont >>= \st -> Transient $
        return $ unsafePerformIO $  runStateT (runTrans mx) st >>=  return .fst

-- log the result a cloud computation. like `loogged`, This eliminated all the log produced by computations
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

--remote :: Loggable a => TransIO a -> Cloud a
--remote x= Cloud $ step' x $ \full x ->  Transient $ do
--            let add= Wormhole: full
--            setData $ Log False add add
--
--            r <-  runTrans x
--
--            let add= WaitRemote: full
--            (setData $ Log False add add)     -- !!> "AFTER STEP"
--            return  r

-- | stop the current computation and does not execute any alternative computation
fullStop :: Cloud stop
fullStop= onAll $ setData WasRemote >> stop


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
callTo node  remoteProc=
   wormhole node $ atRemote remoteProc

#ifndef ghcjs_HOST_OS
-- | A connectionless version of callTo for long running remote calls
callTo' :: (Show a, Read a,Typeable a) => Node -> Cloud a -> Cloud a
callTo' node remoteProc=  do
    mynode <-  local getMyNode
    beamTo node
    r <-  remoteProc
    beamTo mynode
    return r
#endif

-- |  Within an open connection to other node opened by `wormhole`, it run the computation in the remote node and return
-- the result back  to the original node.
atRemote proc= loggedc' $ do
     teleport                    -- !> "teleport 1111"
     r <- Cloud $ runCloud proc <** setData WasRemote
     teleport                    -- !> "teleport 2222"
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
   con@Connection{closChildren=rmap} <- getSData <|> error "single: only works within a wormhole"
   mapth <- liftIO $ readIORef rmap
   id <- liftIO $ makeStableName f >>= return .  hashStableName

--   chs <-
--   let mx =
   case  M.lookup id mapth of
          Just tv ->  liftIO $ killChildren tv    -- !> "JUSTTTTTTTTT"
          Nothing ->  return () --gets children

--   liftIO $  killChildren   chs

--   modify $ \ s -> s{children= chs}  -- to allow his own thread control

   f <** do
      id <- liftIO $ makeStableName f >>= return . hashStableName
      chs <- gets children
      liftIO $ modifyIORef rmap $ \mapth -> M.insert id chs mapth
--      return r

-- | run an unique continuation for each connection. The first thread that execute `unique` is
-- executed for that connection. The rest are ignored.
unique :: a -> TransIO ()
unique f= do
   con@Connection{closChildren=rmap} <- getSData <|> error "unique: only works within a connection. Use wormhole"
   mapth <- liftIO $ readIORef rmap
   id <- liftIO $ f `seq` makeStableName f >>= return .  hashStableName

   let mx = M.lookup id mapth
   case mx of
          Just tv -> empty
          Nothing -> do
             chs <- gets children
             liftIO $ modifyIORef rmap $ \mapth -> M.insert id chs mapth




-- | A wormhole opens a connection with another node anywhere in a computation.
-- `teleport` uses this connection to translate the computation back and forth between the two nodes connected
wormhole :: Loggable a => Node -> Cloud a -> Cloud a
wormhole node (Cloud comp) = local $ Transient $ do

   moldconn <- getData :: StateIO (Maybe Connection)
   mclosure <- getData :: StateIO (Maybe Closure)
   labelState $ "wormhole" ++ show node
   logdata@(Log rec log fulLog) <- getData `onNothing` return (Log False [][])

   mynode <- runTrans getMyNode     -- debug
   if not rec                                    --  !> ("wormhole recovery", rec)
            then runTrans $ (do

                    conn <-  mconnect node
                                                 -- !> ("wormhole",mynode,"connecting node ",  node)
                    setData  conn{calling= True}
-- #ifdef ghcjs_HOST_OS
--                    addPrefix    -- for the DOM identifiers
-- #endif
                    comp )
                  <*** do when (isJust moldconn) . setData $ fromJust moldconn
                          when (isJust mclosure) . setData $ fromJust mclosure

                    -- <** is not enough
            else do
             let conn = fromMaybe (error "wormhole: no connection in remote node") moldconn
--             conn <- getData `onNothing` error "wormhole: no connection in remote node"

             setData $ conn{calling= False}

             runTrans $  comp
                     <*** do
--                          when (null log) $ setData WasRemote    !> "NULLLOG"
                          when (isJust mclosure) . setData $ fromJust mclosure




#ifndef ghcjs_HOST_OS
type JSString= String
pack= id



#endif
--
--newtype Prefix= Prefix JSString deriving(Read,Show)
--{-#NOINLINE rprefix #-}
--rprefix= unsafePerformIO $ newIORef 0
--addPrefix :: (MonadIO m, MonadState EventF m) => m ()
--addPrefix=  do
--   r <- liftIO $ atomicModifyIORef rprefix (\n -> (n+1,n))  -- liftIO $ replicateM  5 (randomRIO ('a','z'))
--   (setData $ Prefix $ pack $ show r) !> "addPrefix"
--



-- | translates computations back and forth between two nodes
-- reusing a connection opened by `wormhole`
--
-- each teleport transport to the other node what is new in the log since the
-- last teleport
--
-- It is used trough other primitives like  `runAt` which involves two teleports:
--
-- runAt node= wormhole node $ loggedc $ do
-- >     teleport
-- >     r <- Cloud $ runCloud proc <** setData WasRemote
-- >     teleport
-- >     return r

teleport ::   Cloud ()
teleport =  do
  local $ Transient $ do

     cont <- get

     -- send log with closure at head
     Log rec log fulLog <- getData `onNothing` return (Log False [][])
     if not rec   -- !> ("teleport rec,loc fulLog=",rec,log,fulLog)
                  -- if is not recovering in the remote node then it is active
      then  do
        conn@Connection{connData=contype,closures= closures,calling= calling} <- getData
             `onNothing` error "teleport: No connection defined: use wormhole"
#ifndef ghcjs_HOST_OS
        case contype of
         Just Self -> do

               setData $ if (not calling) then  WasRemote else WasParallel
               runTrans $ async $ return ()  -- !> "SELF" -- call himself
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
         node <- runTrans getMyNode

         liftIO $ modifyMVar_ closures $ \map -> return $ M.insert closLocal (fulLog,cont) map

         let tosend= reverse $ if closRemote==0 then fulLog else  log


         runTrans $ msend conn $ SMore (closRemote,closLocal,tosend )
--                                    !> ("teleport sending", SMore (closRemote,closLocal,tosend))
--                                    !> "--------->------>---------->"
--                                                  !> ("fulLog", fulLog)

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
putMailbox = putMailbox' 0

-- | write to a mailbox identified by an Integer besides the type
putMailbox' :: Typeable a =>  Int -> a -> TransIO ()
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
getMailbox = getMailbox' 0

-- | read from a mailbox identified by a number besides the type
getMailbox' :: Typeable a => Int -> TransIO a
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
    nodes <-  local getNodes
    let nodes' = filter (not . isWebNode) nodes
    foldr op init $ map (\node -> runAt node proc) nodes'
    where
    isWebNode Node {nodeServices=srvs}
         | ("webnode","") `elem` srvs = True
         | otherwise = False

-----
#ifndef ghcjs_HOST_OS
sendRaw (Connection _(Just (Node2Web  sconn )) _ _ _ _ _ _) r=
      liftIO $   WS.sendTextData sconn  r                                --  !> ("NOde2Web",r)

sendRaw (Connection _(Just (Node2Node _ sock _)) _ _ blocked _ _ _) r=
      liftIO $   withMVar blocked $ const $  SBS.sendMany sock
                                      (BL.toChunks r )                   -- !> ("NOde2Node",r)

sendRaw (Connection _(Just (TLSNode2Node  ctx )) _ _ blocked _ _ _) r=
      liftIO $ withMVar blocked $ const $ sendTLSData ctx  r       --  !> ("TLNode2Web",r)

#else
sendRaw (Connection _ (Just (Web2Node sconn)) _ _ blocked _  _ _) r= liftIO $
   withMVar blocked $ const $ JavaScript.Web.WebSocket.send   r sconn   -- !!> "MSEND SOCKET"
#endif


msend :: Loggable a => Connection -> StreamData a -> TransIO ()

#ifndef ghcjs_HOST_OS

msend (Connection _(Just (Node2Node _ sock _)) _ _ blocked _ _ _) r=
   liftIO $   withMVar blocked $  const $ SBS.sendAll sock $ BC.pack (show r) -- !> "N2N SEND"

msend (Connection _(Just (TLSNode2Node ctx)) _ _ blocked _ _ _) r=
     liftIO $ sendTLSData  ctx $ BS.pack (show r)                             -- !> "TLS SEND"


msend (Connection _(Just (Node2Web sconn)) _ _ blocked _  _ _) r=liftIO $
  {-withMVar blocked $ const $ -} WS.sendTextData sconn $ BS.pack (show r)


#else

msend (Connection _ (Just (Web2Node sconn)) _ _ blocked _  _ _) r= liftIO $
  withMVar blocked $ const $ JavaScript.Web.WebSocket.send  (JS.pack $ show r) sconn   -- !!> "MSEND SOCKET"



#endif

msend (Connection _ Nothing _ _  _ _ _ _) _= error "msend out of wormhole context"

mread :: Loggable a => Connection -> TransIO (StreamData a)


#ifdef ghcjs_HOST_OS


mread (Connection _ (Just (Web2Node sconn)) _ _ _ _  _ _)=  wsRead sconn



wsRead :: Loggable a => WebSocket  -> TransIO  a
wsRead ws= do
  dat <- react (hsonmessage ws) (return ())
  case JM.getData dat of
    JM.StringData str  ->  return (read' $ JS.unpack str)
--                  !> ("Browser webSocket read", str)  !> "<------<----<----<------"
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
mread (Connection _(Just (Node2Node _ _ _)) _ _ _ _ _ _) =  parallelReadHandler -- !> "mread"

mread (Connection _(Just (TLSNode2Node ctx)) _ _ _ _ _ _) =  parallelReadHandler
--        parallel $ do
--            s <- recvTLSData  ctx
--            return . read' $  BC.unpack s

mread (Connection node  (Just (Node2Web sconn )) _ _ _ _ _ _)=
        parallel $ do
            s <- WS.receiveData sconn
            return . read' $  BS.unpack s
--                 !>  ("WS MREAD RECEIVED ---->", s)



getWebServerNode :: TransIO Node
getWebServerNode = getMyNode
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

mclose (Connection _
   (Just (Node2Node _  sock _ )) _ _ _ _ _ _)= NS.sClose sock

mclose (Connection node
   (Just (Node2Web sconn ))
   bufSize events blocked _  _ _)=
    WS.sendClose sconn ("closemsg" :: BS.ByteString)

#else

mclose (Connection _ (Just (Web2Node sconn)) _ _ blocked _ _ _)=
    JavaScript.Web.WebSocket.close Nothing Nothing sconn

#endif



mconnect :: Node -> TransIO  Connection
mconnect  node@(Node _ _ _ _ )=  do
  nodes <- getNodes
--                                                          !> ("connecting node", node)

  let fnode =  filter (==node) nodes
  case fnode of
   [] -> addNodes [node] >> mconnect node
   [Node host port  pool _] -> do


    plist <- liftIO $ readMVar pool
    case plist  of
      handle:_ -> do
                  delData $ Closure undefined
                  return  handle
--                                                            !>   ("REUSED!", node)

      _ -> do
--        liftIO $ putStr "*****CONNECTING NODE: " >> print node
        my <- getMyNode

        Connection{comEvent= ev} <- getSData <|> error "connect: listen not set for this node"

#ifndef ghcjs_HOST_OS

--        conn <- liftIO $ do
        let size=8192
        sock <- liftIO $connectTo'  size  host $ PortNumber $ fromIntegral port
--                                                                                 !> ("CONNECTING ",port)
        conn' <- liftIO $ defConnection >>= \c -> return c{myNode=my,comEvent= ev,connData= Just $ Node2Node u  sock (error $ "addr: outgoing connection")}

        setData conn'
        input <-  liftIO $ SBSL.getContents sock
        maybeClientTLSHandshake (nodeHost node) sock input
        conn <- getSData <|> error "mconnect: no connection data"
        sendRaw conn "CLOS a b\r\n\r\n"

        parseContext <- getSData <|> do
                 input <- liftIO $ SBSL.getContents sock
                 return $ ParseContext (error "listenResponses: Parse error") input


#else

        ws <- connectToWS host $ PortNumber $ fromIntegral port
--                                                           !> "CONNECTWS"
        conn <- defConnection >>= \c -> return c{comEvent= ev,connData= Just $ Web2Node ws}
--                                                           !>  ("websocker CONNECION")
        let parseContext =
                      ParseContext (error "parsecontext not available in the browser")
                        ("" :: JSString)
#endif
        chs <- liftIO $ newIORef M.empty
        let conn'= conn{closChildren= chs}
        liftIO $ modifyMVar_ pool $  \plist -> return $ conn':plist
        putMailbox  (conn',parseContext,node)  -- tell listenResponses to watch incoming responses
        delData $ Closure undefined
        return  conn

  where u= undefined



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
                            ,remoteNode :: NS.SockAddr
                             }
                   | TLSNode2Node{tlscontext :: SData}
                   | Node2Web{webSocket :: WS.Connection}
                   | Self
#else
                   Self
                   | Web2Node{webSocket :: WebSocket}
#endif

data MailboxId= MailboxId Int TypeRep deriving (Eq,Ord)

data Connection= Connection{myNode     :: Node
                           ,connData   :: Maybe(ConnectionData)
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
                           ,closChildren :: IORef (M.Map Int (MVar[EventF]))}

                  deriving Typeable









defConnection :: MonadIO m => m Connection

-- #ifndef ghcjs_HOST_OS
defConnection = liftIO $ do
  x <- newMVar ()
  y <- newMVar M.empty
  z <-  return $ error "closchildren newIORef M.empty"
  return $ Connection (error "node in default connection") Nothing  8192
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
   let conn= conn'{connData=Just Self,myNode=node, comEvent=ev,closChildren=chs}

   setData conn
   liftIO $ modifyMVar_ (connection node) $ const $ return [conn]
   addNodes [node]
   mlog <- listenNew (fromIntegral port) conn  <|> listenResponses

   execLog  mlog

-- listen incoming requests
listenNew port conn'= do

   sock <- liftIO . listenOn  $ PortNumber port

   let bufSize= bufferSize conn'
   liftIO $ do NS.setSocketOption sock NS.RecvBuffer bufSize
               NS.setSocketOption sock NS.SendBuffer bufSize


   (sock,addr) <- waitEvents $ NS.accept sock

   chs <- liftIO $ newIORef M.empty
--   case addr of
--     NS.SockAddrInet port host -> liftIO $ print("connection from", port, host)
--     NS.SockAddrInet6  a b c d -> liftIO $ print("connection from", a, b,c,d)

   let conn= conn'{closChildren=chs}

   input <-  liftIO $ SBSL.getContents sock

   setData $ (ParseContext (error "parsing request") input
             ::ParseContext BS.ByteString)

   initFinish

   onException $ \(e :: SomeException) -> do
--             return()           !> "onFinish closures receivedd with LISTEN" !> me
             liftIO $ print e
             let Connection{closures=closures,closChildren= rmap}= conn
                     -- !> "listenNew closures empty"
             liftIO $ do
                  modifyMVar_ closures $ const $ return M.empty
                  writeIORef rmap M.empty
             topState >>= showThreads
             killBranch

   setState conn{connData=Just (Node2Node (PortNumber port) sock addr)}
   maybeTLSServerHandshake sock input



   (method,uri, headers) <- receiveHTTPHead

   case method of
     "CLOS" ->
          do
           return ()                        --  !> "CLOS detected"
--           setData $ conn{connData=Just (Node2Node (PortNumber port) sock addr)}
           parallelReadHandler

     _ -> do
           let uri'= BC.tail $ uriPath uri
           if  "api/" `BC.isPrefixOf` uri'
             then do


               log <- return $ Exec: (Var $ IDyns $ BS.unpack method):(map (Var . IDyns ) $ split $ BC.unpack $ BC.drop 4 uri')


               str <-  giveData  <|> error "no api data"
--               ParseContext e str <- getSData <|> error "no api context"
               log' <- case (method,lookup "Content-Type" headers) of
                       ("POST",Just "application/x-www-form-urlencoded") -> do
                            len <- read <$>  BC.unpack
                                        <$> (Transient $ return (lookup "Content-Length" headers))
                            setData $ ParseContext (return mempty) $ BS.take len str

                            postParams <- parsePostUrlEncoded  <|> return []
                            return $ log ++  [(Var . IDynamic $ postParams)]

                       _ -> return $ log  -- ++ [Var $ IDynamic  str]

               return $ SMore (0,0, log' )

             else do
                   -- stay serving pages until a websocket request is received
                   sconn <- servePages (method, uri', headers)
                   -- websockets mode
                   setData conn{connData= Just (Node2Web sconn) ,closChildren=chs}
--                   async (return (SMore (0,0,[Exec]))) <|> do
                   do

                     r <-  parallel $ do
                             msg <- WS.receiveData sconn
--                             return () !> ("Server WebSocket msg read",msg)
--                                       !> "<-------<---------<--------------"

                             case reads $ BS.unpack msg of
                               [] -> do
                                   let log =Exec: [Var $ IDynamic  (msg :: BS.ByteString)]
                                   return $ SMore (0,0,log)
                               ((x ,_):_) -> return (x :: StreamData (Int,Int,[LogElem]))

                     case r of
                       SError e -> do
--                           liftIO $ WS.sendClose sconn ("error" :: BS.ByteString)
                           finish (Just e)
--                                                                 !> "FINISH1"
                       _ -> return r

     where
      uriPath = BC.dropWhile (/= '/')
      split []= []
      split ('/':r)= split r
      split s=
          let (h,t) = span (/= '/') s
          in h: split  t



--instance Read PortNumber where
--  readsPrec n str= let [(n,s)]=   readsPrec n str in [(fromIntegral n,s)]


--deriving instance Read PortID
--deriving instance Typeable PortID
#endif

listenResponses :: Loggable a => TransIO (StreamData a)
listenResponses= do
      (conn, parsecontext, node) <- getMailbox
      labelState $ "listen from: "++ show node


      setData conn

#ifndef ghcjs_HOST_OS
      setData (parsecontext :: ParseContext BS.ByteString)
#else
      setData (parsecontext :: ParseContext JSString)
#endif

-- #ifndef ghcjs_HOST_OS
--
--
--      case conn of
--             Connection _(Just (Node2Node _ sock _)) _ _ _ _ _ _ -> do
--                 input <- liftIO $ SBSL.getContents sock
--                 setData $ (ParseContext (error "listenResponses: Parse error") input :: ParseContext BS.ByteString)
--
-- #endif

      cutExceptions
      onException $ \(e:: SomeException) -> do
         liftIO $ putStr "removing node: " >> print node
         nodes <- getNodes
         setNodes $ nodes \\ [node]
         killChilds
         let Connection{closures=closures}= conn
         liftIO $ modifyMVar_ closures $ const $ return M.empty


      mread conn


type IdClosure= Int

newtype Closure= Closure IdClosure deriving Show

execLog  mlog = Transient $
       case  mlog  of
             SError e -> do
                 runTrans $ back e
                 return Nothing

             SDone   -> runTrans(back $ ErrorCall "SDone") >> return Nothing   -- TODO remove closure?
             SMore r -> process r False
             SLast r -> process r True

   where
   process (closl,closr,log) deleteClosure= do
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
                                 runTrans $ msend conn $ SLast (closr,closl, [] :: [()] )
                                    -- to delete the remote closure
                                 error ("request received for non existent closure: "
                                   ++  show closl)
                   -- execute the closure
                   Just (fulLog,cont) -> liftIO $ runStateT (do
                                     let nlog= reverse log ++  fulLog
                                     setData $ Log True  log  nlog

                                     setData $ Closure closr
--                                                                !> ("SETCLOSURE",closr)
                                     runCont cont) cont
--                                                                !> ("executing closure",closl)


                 return Nothing
--                                                                  !> "FINISH CLOSURE"

#ifdef ghcjs_HOST_OS
listen node = onAll $ do
        addNodes [node]

        events <- liftIO $ newIORef M.empty

        conn <-  defConnection >>= \c -> return c{myNode=node,comEvent=events}
        setData conn
        r <- listenResponses
        execLog  r
#endif

type Pool= [Connection]
type Package= String
type Program= String
type Service= (Package, Program)





--------------------------------------------
data ParseContext a = IsString a => ParseContext (IO  a) a deriving Typeable


#ifndef ghcjs_HOST_OS

parallelReadHandler :: Loggable a => TransIO (StreamData a)
parallelReadHandler= do
      str <- giveData :: TransIO BS.ByteString

      choose $ readStream str
--      rest <- liftIO $ newIORef $ BS.unpack str
--
--      r <- parallel $ readStream' rest
--      return r
--                !> ("read",r)
--                !> "<-------<----------<--------<----------"
    where
--    readStream' :: (Loggable a) =>  IORef String  -> IO(StreamData a)
--    readStream' rest = do
--       return () !> "reAD StrEAM"
--       s <- readIORef rest
--       liftIO $ print $ takeWhile (/= ')') s
--       [(x,r)] <- maybeRead  s
--       writeIORef rest r
--       return x

    readStream :: (Typeable a, Read a) =>  BS.ByteString -> [StreamData a]
    readStream s=  readStream1 $ BS.unpack s
     where

     readStream1 s=
       let [(x,r)] = reads  s
       in  x : readStream1 r


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

servePages (method,uri, headers)   = do
--   return ()                        !> ("HTTP request",method,uri, headers)
   conn <- getSData <|> error " servePageMode: no connection"

   if isWebSocketsReq headers
     then  liftIO $ do
         let rec= readFrom conn
             send= sendRaw conn
         stream <- makeStream                  -- !!> "WEBSOCKETS request"
            (do
                bs <-  rec         -- SBS.recv sock 4098
                return $ if BC.null bs then Nothing else Just  bs)
            (\mbBl -> case mbBl of
                Nothing -> return ()
                Just bl ->  send bl) -- SBS.sendMany sock (BL.toChunks bl) >> return())   -- !!> show ("SOCK RESP",bl)

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



     else do

        let file= if BC.null uri then "index.html" else uri
        {- TODO renderin in server
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
            "HTTP/1.0 200 OK\nContent-Type: text/html\nConnection: close\nContent-Length: "
            <> BS.pack (show $ BL.length content) <>"\n\n" <> content
                              --        (BL.toChunks content )
          Nothing ->liftIO $ sendRaw conn $ BS.pack $ "HTTP/1.0 404 Not Found\nContent-Length: 0\nConnection: close\n\n"
        empty


--counter=  unsafePerformIO $ newMVar 0
api :: TransIO BS.ByteString -> Cloud ()
api  w= Cloud  $ do
   conn <- getSData  <|> error "api: Need a connection opened with initNode, listen, simpleWebApp"
   let send= sendRaw conn
   labelState "api"
   r <- w
   liftIO $ myThreadId >>= print
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


createNodeServ ::  HostName -> Integer -> [Service] -> IO Node
createNodeServ h p svs= do
    pool <- emptyPool
    return $ Node h ( fromInteger p) pool svs




createNode :: HostName -> Integer -> IO Node
createNode h p= createNodeServ h p []

createWebNode :: IO Node
createWebNode= do
  pool <- emptyPool
  return $ Node "webnode" ( fromInteger 0) pool  [("webnode","")]


instance Eq Node where
    Node h p _ _ ==Node h' p' _ _= h==h' && p==p'


instance Show Node where
    show (Node h p _ servs )= show (h,p, servs)



instance Read Node where

    readsPrec _ s=
          let r= readsPrec' 0 s
          in case r of
            [] -> []
            [((h,p,ss),s')] ->  [(Node h p empty
              ( ss),s')]
          where
          empty= unsafePerformIO  emptyPool

nodeList :: TVar  [Node]
nodeList = unsafePerformIO $ newTVarIO []

deriving instance Ord PortID

--myNode :: Int -> DBRef  MyNode
--myNode= getDBRef $ key $ MyNode undefined



errorMyNode f= error $ f ++ ": Node not set. initialize it with connect, listen, initNode..."


getMyNode :: TransIO Node -- (MonadIO m, MonadState EventF m) => m Node
getMyNode =  do
    Connection{myNode= node}  <- getSData   <|> errorMyNode "getMyNode" :: TransIO Connection
    return node



-- | return the list of nodes connected to the local node
getNodes :: MonadIO m => m [Node]
getNodes  = liftIO $ atomically $ readTVar  nodeList

-- | add nodes to the list of nodes
addNodes :: [Node] ->  TransIO () -- (MonadIO m, MonadState EventF m) => [Node] -> m ()
addNodes   nodes=  do
  my <- getMyNode    -- mynode must be first
  liftIO . atomically $ do
    prevnodes <- readTVar nodeList

    writeTVar nodeList $ my: (( nub $ nodes ++ prevnodes) \\[my])

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
connect'  remotenode= do
    nodes <- local getNodes
    localIO $ putStrLn $ "connecting to: "++ show remotenode

    newNodes <- runAt remotenode $ do
           local $ do
              conn@Connection{} <- getSData <|>
               error ("connect': need to be connected to a node: use wormhole/connect/listen")
              let nodeConnecting= head nodes
              liftIO $ modifyMVar_ (connection nodeConnecting) $ const $ return [conn]

              onException $ \(e :: SomeException) -> do
                   liftIO $ putStrLn "removing node: ">> print nodeConnecting
                   nodes <- getNodes
                   setNodes $ nodes \\ [nodeConnecting]

--              return nodes   -- delete

           mclustered .  local $ addNodes nodes
           localIO $ return ("after"::String)
           local $ do
               allNodes <- getNodes
               liftIO $ putStrLn "Known nodes: " >> print  allNodes
               return allNodes

    let n = newNodes \\ nodes
    when (not $ null n) $ mclustered $ local $ addNodes n   -- add the new discovered nodes

    local $ do
        addNodes  nodes
        nodes <- getNodes
        liftIO $ putStrLn  "Known nodes: " >> print nodes


#else
connect _ _= empty
connect' _ = empty
#endif






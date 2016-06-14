-----------------------------------------------------------------------------
--
-- Module      :  Transient.Move
-- Copyright   :
-- License     :  GPL-3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- | see <https://www.fpcomplete.com/user/agocorona/moving-haskell-processes-between-nodes-transient-effects-iv>
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable , ExistentialQuantification, OverloadedStrings
    ,ScopedTypeVariables, StandaloneDeriving, RecordWildCards, FlexibleContexts, CPP
    ,GeneralizedNewtypeDeriving #-}
module Transient.Move(

Cloud(..),runCloudIO, runCloudIO',local,onAll,lazy, loggedc, lliftIO,
listen, Transient.Move.connect, connect', fullStop,

wormhole, teleport, copyData,

beamTo, forkTo, streamFrom, callTo, runAt, atRemote,

clustered, mclustered,

newMailbox, putMailbox,getMailbox,cleanMailbox,

#ifndef ghcjs_HOST_OS
setBuffSize, getBuffSize,
#endif

createNode, createWebNode, getMyNode, getNodes,
addNodes, shuffleNodes,

-- * low level

 getWebServerNode, Node(..), nodeList, Connection(..), Service(),
 isBrowserInstance, Prefix(..), addPrefix


) where
import Transient.Base
import Transient.Internals((!>),killChildren,getCont,runCont,EventF(..),LogElem(..),Log(..)
       ,onNothing,RemoteStatus(..),getCont,StateIO,readsPrec')
import Transient.Logged
import Transient.EVars
import Data.Typeable
import Control.Applicative
#ifndef ghcjs_HOST_OS
import Network
import Network.Info
import qualified Network.Socket as NS
import qualified Network.BSD as BSD
import qualified Network.WebSockets as NWS(RequestHead(..))

import qualified Network.WebSockets.Connection   as WS


import Network.WebSockets.Stream   hiding(parse)
import           Data.ByteString       as B             (ByteString,concat)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy as BL
import Network.Socket.ByteString as SBS(send,sendMany,sendAll,recv)
import Data.CaseInsensitive(mk)
import Data.Char(isSpace)
--import GHCJS.Perch (JSString)
#else
import  JavaScript.Web.WebSocket
import  qualified JavaScript.Web.MessageEvent as JM
import GHCJS.Prim (JSVal)
import GHCJS.Marshal(fromJSValUnchecked)
import qualified Data.JSString as JS


import           JavaScript.Web.MessageEvent.Internal
import           GHCJS.Foreign.Callback.Internal (Callback(..))
import qualified GHCJS.Foreign.Callback          as CB
import Data.JSString  (JSString(..), pack)

#endif

import qualified Data.Text as T
import Control.Monad.State
import System.IO
import Control.Exception
import Data.Maybe
import Unsafe.Coerce

--import System.Directory
import Control.Monad

import System.IO.Unsafe
import Control.Concurrent.STM as STM
import Control.Concurrent.MVar

import Data.Monoid
import qualified Data.Map as M
import Data.List (nub,(\\),find, insert)
import Data.IORef


import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO

import Control.Concurrent

import System.Random



import Data.Dynamic
import Data.String


#ifdef ghcjs_HOST_OS
type HostName  = String
newtype PortID = PortNumber Int deriving (Read, Show, Eq, Typeable)
#endif
data Node= Node{ nodeHost   :: HostName
               , nodePort   :: Int
               , connection :: MVar Pool
               , nodeServices   :: MVar [Service]
               }

         deriving (Typeable)

instance Ord Node where
   compare node1 node2= compare (nodeHost node1,nodePort node1)(nodeHost node2,nodePort node2)

-- The cloud monad is a thin layer over Transient in order to make sure that the type system
-- forces the logging of intermediate results
newtype Cloud a= Cloud {runCloud ::TransIO a} deriving (Functor,Applicative,Alternative, Monad, MonadState EventF, Monoid)


--instance Alternative Cloud where
--  empty= Cloud empty
--
--  Cloud x <|>  Cloud y= Cloud $  Transient $ do
--         mx <- runTrans x                -- !!> "RUNTRANS11111"
--         was <- getData `onNothing` return NoRemote
--         if was== WasRemote              -- !> was
--           then return Nothing
--           else case mx of
--                     Nothing -> runTrans y      --  !!> "RUNTRANS22222"
--                     justx -> return justx


-- | Means that this computation will be executed in the current node. the result will be logged
-- so the closure will be recovered if the computation is translated to other node by means of
-- primitives like `beamTo`, `forkTo`, `runAt`, `teleport`, `clustered`, `mclustered` etc
local :: Loggable a => TransIO a -> Cloud a
local =  Cloud . logged

--stream :: Loggable a => TransIO a -> Cloud (StreamVar a)
--stream= Cloud . transport

-- #ifndef ghcjs_HOST_OS
-- | run the cloud computation.
runCloudIO :: Cloud a -> IO a
runCloudIO (Cloud mx)= keep mx

-- | run the cloud computation with no console input
runCloudIO' :: Cloud a -> IO a
runCloudIO' (Cloud mx)= keep' mx

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
loggedc (Cloud mx)= Cloud $ logged mx

lliftIO :: Loggable a => IO a -> Cloud a
lliftIO= local . liftIO

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

-- |  Within an open connection to other node opened by `wormhole`, it run the computation in the remote node and return
-- the result back  to the original node.
atRemote proc= loggedc $ do
     teleport                   -- !> "teleport 1111"
     r <- Cloud $ runCloud proc <** setData WasRemote
     teleport                  --  !> "teleport 2222"
     return r

-- | synonymous of `callTo`
runAt :: Loggable a => Node -> Cloud a -> Cloud a
runAt= callTo


msend :: Loggable a => Connection -> StreamData a -> TransIO ()


#ifndef ghcjs_HOST_OS

msend (Connection _(Just (Node2Node _ h sock)) _ _ blocked _ _ ) r= do
  r <- liftIO $ do
       withMVar blocked $
             const $ do
                 hPutStrLn h (show r)
                 hFlush h  >> return Nothing `catch` (\(e::SomeException) -> return $ Just e)
  case r of
      Nothing -> return()
      juste -> finish juste


msend (Connection _(Just (Node2Web sconn)) _ _ blocked _  _) r=liftIO $
  withMVar blocked $ const $ WS.sendTextData sconn $ BS.pack (show r)


#else

msend (Connection _ (Just (Web2Node sconn)) _ _ blocked _  _) r= liftIO $
  withMVar blocked $ const $ JavaScript.Web.WebSocket.send  (JS.pack $ show r) sconn   -- !!> "MSEND SOCKET"



#endif

msend (Connection _ Nothing _ _  _ _ _ ) _= error "msend out of wormhole context"

mread :: Loggable a => Connection -> TransIO (StreamData a)
#ifdef ghcjs_HOST_OS


mread (Connection _ (Just (Web2Node sconn)) _ _ _ _  _)=  wsRead sconn



wsRead :: Loggable a => WebSocket  -> TransIO  a
wsRead ws= do
  dat <- react (hsonmessage ws) (return ())
  case JM.getData dat of
    JM.StringData str  ->  return (read' $ JS.unpack str)
--                                    !> ("Browser webSocket read", str)  !> "<------<----<----<------"
    JM.BlobData   blob -> error " blob"
    JM.ArrayBufferData arrBuffer -> error "arrBuffer"


{-
wsRead1 :: Loggable a => WebSocket  -> TransIO (StreamData a)
wsRead1 ws= do
  reactStream (makeCallback MessageEvent) (js_onmessage ws) CB.releaseCallback (return ())
  where
  reactStream createHandler setHandler removeHandler iob= Transient $ do
        cont    <- getCont
        hand <- liftIO . createHandler $ \dat ->do
              runStateT (setData dat >> runCont cont) cont
              iob
        mEvData <- getSessionData
        case mEvData of
          Nothing -> liftIO $ do
                        setHandler hand
                        return Nothing

          Just dat -> do
             liftIO $ print "callback called 2*****"
             delSessionData dat
             dat' <- case getData dat of
                 StringData str  -> liftIO $ putStrLn "WSREAD RECEIVED " >> print str >> return (read $ JS.unpack str)
                 BlobData   blob -> error " blob"
                 ArrayBufferData arrBuffer -> error "arrBuffer"
             liftIO $ case dat' of
               SDone -> do
                        removeHandler $ Callback hand
                        empty
               sl@(SLast x) -> do
                        removeHandler $ Callback hand     -- !!> "REMOVEHANDLER"
                        return $ Just sl
               SError e -> do
                        removeHandler $ Callback hand
                        print e
                        empty
               more -> return (Just  more)
-}


wsOpen :: JS.JSString -> TransIO WebSocket
wsOpen url= do
   ws <-  liftIO $ js_createDefault url      --  !> ("wsopen",url)
   react (hsopen ws) (return ())             -- !!> "react"
   return ws                                 -- !!> "AFTER ReACT"

foreign import javascript safe
    "window.location.hostname"
   js_hostname ::    JSVal

foreign import javascript safe
   "(function(){var res=window.location.href.split(':')[2];if (res === undefined){return 80} else return res.split('/')[0];})()"
   js_port ::   JSVal

foreign import javascript safe
    "$1.onmessage =$2;"
   js_onmessage :: WebSocket  -> JSVal  -> IO ()

getWebServerNode _=

    createNode  <$> ( fromJSValUnchecked js_hostname)
                <*> (fromIntegral <$> (fromJSValUnchecked js_port :: IO Int))

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
mread (Connection _(Just (Node2Node _ h _)) _ _ blocked _ _ ) =  parallel $ readHandler  h


mread (Connection node  (Just (Node2Web sconn )) bufSize events blocked _ _)=
        parallel $ do
            s <- WS.receiveData sconn
            return . read' $  BS.unpack s          !>  ("WS MREAD RECEIVED ---->", s)

--           `catch`(\(e ::SomeException) -> return $ SError e)

getWebServerNode port= return $ createNode "localhost" port
#endif

read' s= case readsPrec' 0 s of
       [(x,"")] -> x
       _  -> error $ "reading " ++ s

-- | A wormhole opens a connection with another node anywhere in a computation.
-- `teleport` uses this connection to translate the computation back and forth between the two nodes
wormhole :: Loggable a => Node -> Cloud a -> Cloud a
wormhole node (Cloud comp) = local $ Transient $ do

   moldconn <- getData :: StateIO (Maybe Connection)
   mclosure <- getData :: StateIO (Maybe Closure)

   logdata@(Log rec log fulLog) <- getData `onNothing` return (Log False [][])

   mynode <- runTrans getMyNode     -- debug
   if not rec                                                    --  !> ("wormhole recovery", rec)
            then runTrans $ (do

                    conn <-  mconnect node                   --  !> (mynode,"connecting node ",  node)
                    setData  conn{calling= True}
#ifdef ghcjs_HOST_OS
                    addPrefix    -- for the DOM identifiers
#endif
                    comp )
                  <*** do when (isJust moldconn) . setData $ fromJust moldconn
                          when (isJust mclosure).  setData $ fromJust mclosure

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

newtype Prefix= Prefix JSString deriving(Read,Show)
addPrefix= Transient $ do
   r <- liftIO $ replicateM  5 (randomRIO ('a','z'))
   setData $ Prefix $ pack  r
   return $ Just ()


-- | translates computations back and forth
-- reusing a connection opened by `wormhole`
teleport ::   Cloud ()
teleport =  do
  local $ Transient $ do

     cont <- get

     -- send log with closure at head
     Log rec log fulLog <- getData `onNothing` return (Log False [][])
     if not rec    -- !> ("teleport rec,loc fulLog=",rec,log,fulLog)
                  -- if is not recovering in the remote node then it is active
      then  do
         conn@Connection{closures= closures,calling= calling} <- getData
             `onNothing` error "teleport: No connection defined: use wormhole"

         --read this Closure
         Closure closRemote  <- getData `onNothing` return (Closure 0 )

         --set his own closure in his Node data

         let closLocal = sum $ map (\x-> case x of Wait-> 1000; _ -> 1) fulLog
--         closLocal  <-   liftIO $ randomRIO (0,1000000)

         liftIO $ modifyMVar_ closures $ \map -> return $ M.insert closLocal (fulLog,cont) map

         let tosend= reverse $ if closRemote==0 then fulLog else  log -- drop offset  $ reverse fulLog  !> ("fulLog", fulLog)

         runTrans $ msend conn $ SMore (closRemote,closLocal, tosend )
--                                                  !> ("teleport sending", tosend )
--                                                  !> "--------->------>---------->"

         setData $ if (not calling) then  WasRemote else WasParallel

         return Nothing

      else do

         delData WasRemote                -- !> "deleting wasremote in teleport"
                                          -- it is recovering, therefore it will be the
                                          -- local, not remote

         return (Just ())                           --  !> "TELEPORT remote"




--   -- save the state of the web rendering variables if needed
--   saveVars= runTrans . runCloud $ do
--     sav <- local $ ((getSData :: TransIO ( Repeat)) >> return True) <|> return False
--     when sav  $ do
--       let copyCounter= do
--               r <- local $ gets mfSequence
--               onAll $ modify $ \s -> s{mfSequence= r}
--
--       copyData $ Prefix ""                -- !> "SAVE"
--       copyData $ IdLine ""
--       copyData  Repeat
--       copyCounter
--


-- | copy a session data variable from the local to the remote node.
-- The parameter is the default value, if there is none set in the local node.
-- Then the default value is also set in the local node.
copyData def = do
  r <- local getSData <|> return def
  onAll $ setData r
  return r

-- | `callTo` can stream data but can not inform the receiving process about the finalization. This call
-- does it.
streamFrom :: Loggable a => Node -> Cloud (StreamData a) -> Cloud  (StreamData a)
streamFrom = callTo


--release (Node h p rpool _) hand= liftIO $ do
----    print "RELEASED"
--    atomicModifyIORef rpool $  \ hs -> (hand:hs,())
--      -- !!> "RELEASED"

mclose :: Connection -> IO ()

#ifndef ghcjs_HOST_OS

mclose (Connection _
   (Just (Node2Node _ h sock )) _ _ _ _ _)= hClose h

mclose (Connection node
   (Just (Node2Web sconn ))
   bufSize events blocked _  _)=
    WS.sendClose sconn ("closemsg" :: ByteString)

#else

mclose (Connection _ (Just (Web2Node sconn)) _ _ blocked _ _)=
    JavaScript.Web.WebSocket.close Nothing Nothing sconn

#endif

mconnect :: Node -> TransIO  Connection
mconnect  node@(Node _ _ _ _ )=  do
  nodes <- getNodes

  let fnode =  filter (==node) nodes
  case fnode of
   [] -> addNodes [node] >> mconnect node
   [Node host port  pool _] -> do
--    Log _ _ full <- getSData <|> error "mconnect log error"

    plist <- liftIO $ readMVar pool
    case plist  of
      handle:_ -> do
                  delData $ Closure undefined
                  return  handle                        -- !>   "REUSED!"

      _ -> do
        liftIO $ putStr "*****CONNECTING NODE: " >> print node
        my <- getMyNode
--        liftIO  $ putStr "OPENING CONNECTION WITH :" >> print port
        Connection{comEvent= ev} <- getSData <|> error "connect: listen not set for this node"
#ifndef ghcjs_HOST_OS

        conn <- liftIO $ do
          let size=8192
          h <-  connectTo' size host $ PortNumber $ fromIntegral port   -- !!> ("CONNECTING "++ show port)
          hSetBuffering h $ BlockBuffering $ Just size

          let conn= (defConnection 8100){myNode= my,comEvent= ev,connData= Just $ Node2Node u h u}
          hPutStrLn   h "LOG a b"
          hPutStrLn   h ""

          return conn
#else
        conn <- do
          ws <- connectToWS host $ PortNumber $ fromIntegral port
          let conn= (defConnection 8100){comEvent= ev,connData= Just $ Web2Node ws}

          return conn
#endif
        liftIO $ modifyMVar_ pool $  \plist -> return $ conn:plist

        putMailbox "connections" (conn,node)

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
              he <- BSD.getHostByName hostname
              NS.connect sock (NS.SockAddrInet port (BSD.hostAddress he))

              NS.socketToHandle sock ReadWriteMode
            )
#else
connectToWS  h (PortNumber p) =
   wsOpen $ JS.pack $ "ws://"++ h++ ":"++ show p
#endif
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

type Blocked= MVar ()
type BuffSize = Int
data ConnectionData=
#ifndef ghcjs_HOST_OS
                   Node2Node{port :: PortID
                              ,handle :: Handle
                              ,socket ::Socket
                                   }

                   | Node2Web{webSocket :: WS.Connection}
#else

                   Web2Node{webSocket :: WebSocket}
#endif



data Connection= Connection{myNode :: Node
                           ,connData :: (Maybe(ConnectionData))
                           ,bufferSize ::BuffSize
                           ,comEvent :: IORef (M.Map T.Text (EVar Dynamic))
                           ,blocked :: Blocked
                           ,calling :: Bool
                           ,closures   :: MVar (M.Map Int ([LogElem], EventF))}

                  deriving Typeable


-- Mailboxes are node-wide, for all processes that share the same connection data, that is, are under the
-- same `listen`  or `connect`
-- while EVars are only visible by the process that initialized  it and his children.
-- Internally, the mailbox is in a well known EVar stored by `listen` in the `Connection` state.
newMailbox :: T.Text -> TransIO ()
newMailbox name= do  -- liftIO $ replicateM  10 (randomRIO ('a','z'))
   return () !> "newMailBox"
   Connection{comEvent= mv} <- getData `onNothing` errorMailBox
   onFinish . const $ liftIO $ do print "newMailbox finisn" ; atomicModifyIORef mv $ \mailboxes ->   (M.delete name  mailboxes,())
   ev <- newEVar
   liftIO $ atomicModifyIORef mv $ \mailboxes ->   (M.insert name ev mailboxes,())


-- | write tot he mailbox
putMailbox :: Typeable a => T.Text -> a -> TransIO ()
putMailbox name dat= do --  sendNodeEvent (name, Just dat)
   Connection{comEvent= mv} <- getData `onNothing` errorMailBox
   mbs <- liftIO $ readIORef mv
   let mev =  M.lookup name mbs
   case mev of
     Nothing ->newMailbox name >> putMailbox name dat
     Just ev -> writeEVar ev $ toDyn dat

errorMailBox= error "MailBox: No connection open.Use wormhole"

-- | get messages from the mailbox that matches with the type expected.
-- The order of reading is defined by `readTChan`
-- This is reactive. it means that each new message trigger the execution of the continuation
-- each message wake up all the `getMailbox` computations waiting for it.

getMailbox name= do
   Connection{comEvent= mv} <- getData `onNothing` errorMailBox
   mbs <- liftIO $ readIORef mv
   let mev =  M.lookup name mbs
   case mev of
     Nothing ->newMailbox name >> getMailbox name
     Just ev ->do
          d <- readEVar ev
          case fromDynamic d  !> "getMailBox" of
             Nothing -> empty
             Just x -> return x

--getMailBox :: Typeable a => String -> TransIO a
--getMailBox name= Transient $ do
--     return () !> "getMailBox"
--     Connection{comEvent=(EVar id rn ref1)} <- getData `onNothing` error "getMailBox: accessing network events out of listen"
--     runTrans  $ do
--         liftIO $ atomically $ readTVar rn >>= \(n,n') -> writeTVar rn (n +1,n'+1)
--         r <- parallel $  do
--                  n' <- atomically $ do
--                              (n,n') <- readTVar rn
--                              writeTVar rn $ if n' > 1 then   (n,n'-1) else (n, n)
--                              return n'
--                  return () !> ("rear rn",name,n')
--                  atomically $ do
--                    d <- if n'> 1 then  peekTChan ref1 else readTChan ref1
--
--
--                    return () !> ("readmailbox",name,n')
--                    case d of
--                        SDone -> return SDone
--                        SMore x ->  case fromDynamic x of
--                              Nothing -> return $ SMore Nothing
--                              Just (nam, Nothing) -> do
--                                readTVar rn >>= \(n,n') -> writeTVar rn $ (n,n' -1)
--                                return SDone
--                              Just (nam,Just dat) ->
--                                return $ SMore $  if nam /= name then  Nothing else Just dat
----                        SLast x -> case fromDynamic x of
----                              Nothing -> retry
----                              Just (nam, Nothing) -> return SDone
----                              Just (nam,Just dat) ->
----                                if nam /= name !>(nam,name) then retry else return $ SLast dat
--                        SError e -> return $ SError e
--
--         case r of
--            SDone -> empty
--            SMore Nothing -> empty
--            SMore (Just x) -> return x
--            SLast (Just x) -> return x
--            SError e -> error $ show e

-- | delete all subscriptions for that mailbox expecting this kind of data.
cleanMailbox :: Typeable a =>  T.Text ->  a -> TransIO ()
cleanMailbox name witness= do
   Connection{comEvent= mv} <- getData `onNothing` error "getMailBox: accessing network events out of listen"
   mbs <- liftIO $readIORef mv
   let mev =  M.lookup name mbs
   case mev of
     Nothing -> return()
     Just ev -> do cleanEVar ev
                   liftIO $ atomicModifyIORef mv $ \mbs -> (M.delete name mbs,())

--Transient $ do
--   Connection{comEvent=(EVar id rn ref1) }<- getData
--          `onNothing` error "sendNodeEvent: accessing network events out of listen"
--   runTrans $ liftIO $ atomically $ do
--        readTVar rn >>= \(n,n') -> writeTVar rn $ (n -1, n'-1)
--        writeTChan  ref1 $ SMore $ toDyn ( name,Nothing `asTypeOf` Just witness)


---- | updates the local mailbox.
--sendNodeEvent :: Typeable a => a -> TransIO ()
--sendNodeEvent dat=  Transient $ do
--   Connection{comEvent=comEvent}<- getData
--          `onNothing` error "sendNodeEvent: accessing network events out of listen"
--   (runTrans $  writeEVar comEvent $ toDyn dat)  -- !> "PUTMAILBOXX"


-- | wait until a message of the type expected appears in the mailbox. Then executes the continuation
-- When the message appears, all the waiting `waitNodeEvents` are executed from newer to the older
-- following the `readEVar` order.
--waitNodeEvents :: Typeable a => TransIO a
--waitNodeEvents = Transient $  do
--       Connection{comEvent=(EVar id rn ref1)} <- getData `onNothing` error "waitNodeEvents: accessing network events out of listen"
--       runTrans  $ do
----                     d <-  readEVar comEvent
----                     case fromDynamic d of
----                      Nothing -> empty
----                      Just x ->  return x
--         liftIO $ atomically $ readTVar rn >>= \(n,n') ->  writeTVar rn $ (n+1,n'+1)
--         r <- parallel $ atomically $ do
--                    (n,n') <- readTVar rn
--
--                    r <- if n'> 1  then do
--                               r <- peekTChan ref1
--                               writeTVar rn (n,n'-1)
--                               return r
--                             else  do
--                               r <- readTChan ref1
--                               writeTVar rn (n,n)
--                               return r
--                    case r of
--
--                        SMore d -> case fromDynamic d of
--                                      Nothing -> retry
--                                      Just x ->  return $ SMore x
--                        SError e -> return $ SError e
--                        SDone -> return SDone
----                        elsee -> return elsee
--
--         case r of
--            SDone -> empty
--            SMore x -> return x
--
--            SError e -> error $ show e
--
--
--
---- | delete all the event watchers of the node
--cleanNodeEvents=Transient $  do
--       Connection{comEvent=comEvent} <- getData `onNothing` error "waitNodeEvents: accessing network events out of listen"
--       runTrans  $ cleanEVar comEvent



defConnection :: Int -> Connection

-- #ifndef ghcjs_HOST_OS
defConnection size=
 Connection (createNode "program" 0) Nothing  size
                 (error "defConnection: accessing network events out of listen")
                 (unsafePerformIO $ newMVar ())
                 False (unsafePerformIO $ newMVar M.empty)



#ifndef ghcjs_HOST_OS
setBuffSize :: Int -> TransIO ()
setBuffSize size= Transient $ do
   conn<- getData `onNothing` return (defConnection 8192)
   setData $ conn{bufferSize= size}
   return $ Just ()

getBuffSize=
  (do getSData >>= return . bufferSize) <|> return  8192

readHandler h= do
    line <- hGetLine h
--    return ()                             !> ("socket read",line) !> "------<---------------<----------<"
    let [(v,left)] = readsPrec' 0 line
    return  v
--   `catch` (\(e::SomeException) ->  return $ SError e)




listen ::  Node ->  Cloud ()
listen  (node@(Node _  ( port) _ _ )) = onAll $ do
   addThreads 1
   addNodes [node]
   setData $ Log False [] []

   conn' <- getSData <|> return (defConnection 8192)
   ev <- liftIO $ newIORef M.empty
   let conn= conn'{myNode= node , comEvent=ev}
   setData conn

   mlog <- listenNew (fromIntegral port) conn  <|> listenResponses


   checkLog mlog



listenNew port conn= do --  node bufSize events blocked port= do


   sock <- liftIO . listenOn  $ PortNumber port

   let bufSize= bufferSize conn
   liftIO $ do NS.setSocketOption sock NS.RecvBuffer bufSize
               NS.setSocketOption sock NS.SendBuffer bufSize


   st <- getCont
   (sock,addr) <- waitEvents $  NS.accept sock         -- !!> "BEFORE ACCEPT"
--   case addr of
--     NS.SockAddrInet port host -> liftIO $ print("connection from", port, host)
--     NS.SockAddrInet6  a b c d ->
--                                    liftIO $ print("connection from", a, b,c,d)

   h <- liftIO $ NS.socketToHandle sock ReadWriteMode      -- !!> "NEW SOCKET CONNECTION"


   onFinish $ const $ do
             let Connection{closures=closures}= conn !> "listenNew closures empty"
             liftIO $ modifyMVar_ closures $ const $ return M.empty


   (method,uri, headers) <- receiveHTTPHead h

   case method of
     "LOG" ->
          do
           setData $ conn{connData=Just (Node2Node (PortNumber port) h sock )}
--           setData $ Connection node  (Just (Node2Node (PortNumber port) h sock ))
--                         bufSize events blocked False True 0
           killOnFinish $ parallel $ readHandler  h        -- !> "read Listen"  -- :: TransIO (StreamData [LogElem])

     _ -> do
           sconn <- httpMode (method,uri, headers) sock

           setData conn{connData= Just (Node2Web sconn )}

--           setData $ (Connection node  (Just (Node2Web sconn ))
--                         bufSize events blocked False True 0 :: Connection)

           killOnFinish $ parallel $ do
               msg <- WS.receiveData sconn             -- WebSockets
               return . read $ BC.unpack msg
--                                                      !> ("Server WebSocket msg read",msg)  !> "<-------<---------<--------------"







instance Read PortNumber where
  readsPrec n str= let [(n,s)]=   readsPrec n str in [(fromIntegral n,s)]


deriving instance Read PortID
deriving instance Typeable PortID
#endif


listenResponses= do

      (conn, node) <- getMailbox "connections"
      setData conn


      onFinish $ const $ do
--           plist <- liftIO $ readMVar pool
--           case plist of
--            [_] -> do
             liftIO $ putStrLn "removing node: ">> print node
             nodes <- getNodes
             setNodes $ nodes \\ [node]


             let Connection{closures=closures}= conn
             liftIO $ modifyMVar_ closures $ const $ return M.empty


      killOnFinish $ mread conn


type IdClosure= Int

data Closure= Closure IdClosure
checkLog mlog = Transient $ do
       case  mlog    of                       -- !> ("RECEIVED ", mlog ) of
             SError e -> do
                 runTrans $ finish $ Just e
                 return Nothing

             SDone   -> runTrans(finish Nothing) >> return Nothing
             SMore r -> process r False
             SLast r -> process r True

   where
   process (closl,closr,log) deleteClosure= do
              Connection {closures=closures} <- getData `onNothing` error "Listen: myNode not set"

              if closl== 0 then do
                   setData $ Log True log  $ reverse log
                   setData $ Closure closr
                   return $ Just ()                        --  !> "executing top level closure"

               else do
                 mcont <- liftIO $ modifyMVar closures  $ \map ->
                                               return (if deleteClosure then
                                                           M.delete closl map
                                                         else map, M.lookup closl map)
                                                           -- !> ("closures=", M.size map)
                 case mcont of
                   Nothing -> error ("received non existent closure: " ++  show closl)
                   -- execute the closure
                   Just (fulLog,cont) -> liftIO $ runStateT (do
                                     let nlog= reverse log ++  fulLog -- dropWhile (\x -> case x of
                                                                        --  Exec -> True
                                                                        --  _ -> False) fulLog
                                     setData $ Log True  log  nlog

                                     setData $ Closure closr
                                     runCont cont) cont   -- !> ("executing closure",closr)


                 return Nothing                     -- !> "FINISH CLOSURE"

#ifdef ghcjs_HOST_OS
listen node = onAll $ do
        addNodes [node]
        events <- liftIO $ newIORef M.empty
        let conn=  (defConnection 8192){myNode= node,comEvent=events}--  Connection node Nothing 8192 events (unsafePerformIO $ newMVar ()) False False 0 :: Connection xxxxxx
        setData conn
        r <- listenResponses
        checkLog r
#endif

type Pool= [Connection]
type Package= String
type Program= String
type Service= (Package, Program, Int)


-- * Level 2: connections node lists and operations with the node list


{-# NOINLINE emptyPool #-}
emptyPool :: MonadIO m => m (MVar Pool)
emptyPool= liftIO $ newMVar  []

createNode :: HostName -> Integer -> Node
createNode h p= Node h ( fromInteger p) (unsafePerformIO emptyPool)
                 ( unsafePerformIO $ newMVar [])


createWebNode= Node "webnode" ( fromInteger 0) (unsafePerformIO emptyPool)
                 ( unsafePerformIO $ newMVar [("webnode","",0)])


instance Eq Node where
    Node h p _ _ ==Node h' p' _ _= h==h' && p==p'


instance Show Node where
    show (Node h p _ servs )= show (h,p,unsafePerformIO $ readMVar servs)


instance Read Node where

    readsPrec _ s=
          let r= readsPrec' 0 s
          in case r of
            [] -> []
            [((h,p,ss),s')] ->  [(Node h p empty
              (unsafePerformIO $ newMVar ss),s')]
          where
          empty= unsafePerformIO  emptyPool


--newtype MyNode= MyNode Node deriving(Read,Show,Typeable)


--instance Indexable MyNode where key (MyNode Node{nodePort=port}) =  "MyNode "++ show port
--
--instance Serializable MyNode where
--    serialize= BS.pack . show
--    deserialize= read . BS.unpack

nodeList :: TVar  [Node]
nodeList = unsafePerformIO $ newTVarIO []

deriving instance Ord PortID

--myNode :: Int -> DBRef  MyNode
--myNode= getDBRef $ key $ MyNode undefined






errorMyNode f= error $ f ++ ": Node not set. Use initConnData before listen"

-- #ifdef ghcjs_HOST_OS
--getMyNode :: TransIO Node
--getMyNode= return createWebNode
--
----initConnData :: Node -> TransIO ()
----initConnData node= do
----        addNodes [node]
----        events <- newEVar
----        let conn= Connection () Nothing 8192 events (unsafePerformIO $ newMVar ()) False False 0 :: Connection
----        setData conn
-- #else

getMyNode :: TransIO Node
getMyNode =  do
    Connection{myNode=node} <- getSData <|> errorMyNode "getMyNode"

    return node



--initConnData :: Node -> TransIO ()
--initConnData node= do
--        addNodes [node]
--        events <- newEVar
--        let conn= Connection node Nothing 8192 events (unsafePerformIO $ newMVar ()) False False 0  :: Connection
--        setData conn
--        return $ Just ()


-- | return the list of nodes connected to the local node
getNodes :: MonadIO m => m [Node]
getNodes  = liftIO $ atomically $ readTVar  nodeList

-- | add nodes to the list of nodes
addNodes :: (MonadIO m, MonadState EventF m) => [Node] -> m ()
addNodes   nodes=  do

-- #ifndef ghcjs_HOST_OS
--  mapM_ verifyNode nodes -- if the node is a web one, add his connection
-- #endif

  liftIO . atomically $ do
    prevnodes <- readTVar nodeList
    writeTVar nodeList $ nub $ prevnodes  ++ nodes

-- | set the list of nodes
setNodes nodes= liftIO $ atomically $ writeTVar nodeList $  nodes

-- #ifndef ghcjs_HOST_OS
--verifyNode (WebNode pool)= do
--  r <- getData `onNothing` error "adding web node without connection set"
--  case r of
--   conn@(Connection{connData= Just( Node2Web ws)}) ->
--            liftIO $ writeIORef pool [conn]
--   other -> return ()
--
--verifyNode n= return ()
-- #endif

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
clustered proc= loggedc $ do
    nodes <-  local getNodes

    let nodes' = filter (not . isWebNode) nodes
    foldr (<|>) empty $ map (\node -> runAt node proc) nodes'  -- !> ("clustered",nodes')
    where
    isWebNode Node {nodeServices=srvs}
         | ("webnode","",0) `elem` (unsafePerformIO $ readMVar srvs)= True
         | otherwise = False


-- A variant of `clustered` that wait for all the responses and `mappend` them
mclustered :: (Monoid a, Loggable a)  => Cloud a -> Cloud a
mclustered proc= loggedc $ do
    nodes <-  local getNodes
    let nodes' = filter (not . isWebNode) nodes
    foldr (<>) mempty $ map (\node -> runAt node proc) nodes'  -- !> ("mclustered",nodes')
    where
    isWebNode Node {nodeServices=srvs}
         | ("webnode","",0) `elem` (unsafePerformIO $ readMVar srvs)= True
         | otherwise = False



-- | set the rest of the computation as a new node (first parameter) and connect it
-- to an existing node (second parameter). then it uses `connect`` to synchronize the list of nodes
connect ::  Node ->  Node -> Cloud ()
connect  node  remotenode =   do
    listen node <|> return () -- listen1 node remotenode
    connect' remotenode

-- | synchronize the list of nodes with a remote node and all the nodes connected to it
-- the final effect is that all the nodes share the same list of nodes
connect'  remotenode= do
    nodes <- local $ getNodes
    local $ liftIO $ putStrLn $ "connecting to: "++ show remotenode
--    newnode <- local $ return node -- must pass my node to the remote node or else it will use his own

    nodes' <- runAt remotenode $  do
                   mclustered $ local $ addNodes nodes
                   local $ do
                      liftIO $ putStrLn  "New  nodes: " >> print nodes
                      n <- getNodes
                      my <- getMyNode
                      return $ n \\ [my]

    let newNodes = remotenode : nodes'
    let n = newNodes \\ nodes
    when (not $ null n) $ mclustered $ local $ do
          liftIO $ putStrLn  "New  nodes: " >> print n
          addNodes n   -- add the new discovered nodes to the already known ones.
    local $ do
       addNodes  nodes
       nodes <- getNodes
       liftIO $ putStrLn  "Known nodes: " >> print nodes


--------------------------------------------


#ifndef ghcjs_HOST_OS
httpMode (method,uri, headers) conn  = do
   if isWebSocketsReq headers
     then  liftIO $ do

         stream <- makeStream                  -- !!> "WEBSOCKETS request"
            (do
                bs <- SBS.recv conn  4096
                return $ if BC.null bs then Nothing else Just bs)
            (\mbBl -> case mbBl of
                Nothing -> return ()
                Just bl ->  SBS.sendMany conn (BL.toChunks bl) >> return())   -- !!> show ("SOCK RESP",bl)

         let
             pc = WS.PendingConnection
                { WS.pendingOptions     = WS.defaultConnectionOptions
                , WS.pendingRequest     =  NWS.RequestHead uri headers False -- RequestHead (BC.pack $ show uri)
                                                      -- (map parseh headers) False
                , WS.pendingOnAccept    = \_ -> return ()
                , WS.pendingStream      = stream
                }


         sconn    <- WS.acceptRequest pc               -- !!> "accept request"
         WS.forkPingThread sconn 30
         return sconn



     else do
          let uri'= BC.tail $ uriPath uri               -- !> "HTTP REQUEST"
              file= if BC.null uri' then "index.html" else uri'

          content <- liftIO $  BL.readFile ( "./static/out.jsexe/"++ BC.unpack file)
                            `catch` (\(e:: SomeException) -> return  "Index.html NOT FOUND")
          return ()
          n <- liftIO $ SBS.sendMany conn   $  ["HTTP/1.0 200 OK\nContent-Type: text/html\nConnection: close\nContent-Length: " <> BC.pack (show $ BL.length content) <>"\n\n"] ++
                                  (BL.toChunks content )
          return ()    -- !> "HTTP sent"
          empty

      where
      uriPath = BC.dropWhile (/= '/')



isWebSocketsReq = not  . null
    . filter ( (== mk "Sec-WebSocket-Key") . fst)



data ParseContext a = ParseContext (IO  a) a deriving Typeable


--giveData h= do
--    r <- BC.hGetLine h
--    return r !> ( "RECEIVED "++ show r)

giveData h= do

   r <- readIORef rend

   if r then return "" else do
    r<- BC.hGetLine h                    -- !!> "GETLINE"

    if r=="\r" || r == "" then do
       writeIORef rend True
       return ""
       else return r
  where
  rend= unsafePerformIO $ newIORef False


receiveHTTPHead h = do
  setData $ ParseContext (giveData h) ""
  (method, uri, vers) <- (,,) <$> getMethod <*> getUri <*> getVers
  headers <- many $ (,) <$> (mk <$> getParam) <*> getParamValue    -- !>  (method, uri, vers)
  return (method, uri, headers)                                    -- !>  (method, uri, headers)

  where

  getMethod= getString
  getUri= getString
  getVers= getString
  getParam= do
      dropSpaces
      r <- tTakeWhile (\x -> x /= ':' && x /= '\r')
      if BC.null r || r=="\r"  then  empty  else  dropChar >> return r

  getParamValue= dropSpaces >> tTakeWhile  (/= '\r')

  dropSpaces= parse $ \str ->((),BC.dropWhile isSpace str)

  dropChar= parse  $ \r -> ((), BC.tail r)

  getString= do
    dropSpaces

    tTakeWhile (not . isSpace)

  tTakeWhile :: (Char -> Bool) -> TransIO BC.ByteString
  tTakeWhile cond= parse (BC.span cond)

  parse :: (Typeable a, Eq a, Show a, Monoid a,Monoid b) => (a -> (b,a)) -> TransIO b
  parse split= do
    ParseContext rh str <- getSData <|> error "parse: ParseContext not found"
    if  str == mempty then do
          str3 <- liftIO  rh

          setData $ ParseContext rh str3                     -- !> str3

          if str3== mempty then empty   else  parse split

       else do

          cont <- do
             let (ret,str3) = split str
             setData $ ParseContext rh str3
             if  str3 == mempty
                then  return $ Left ret
                else  return $ Right ret
          case cont of
            Left r  ->  (<>) <$> return r  <*> (parse split <|> return mempty)

            Right r ->   return r




#endif



#ifdef ghcjs_HOST_OS
isBrowserInstance= True

#else
isBrowserInstance= False
#endif

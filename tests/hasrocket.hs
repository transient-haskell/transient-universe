#!/usr/bin/env ./execthirdline.sh
-- compile and run within a docker image
-- set -e && executable=`basename -s .hs ${1}` &&  docker run -it -v $(pwd):/work agocorona/transient:04-02-2017  bash -c "cabal install mono-traversable unagi-chan && ghc /work/${1} && /work/${executable} ${2} ${3}"


-- transient application for the websocket shootout
-- https://github.com/hashrocket/websocket-shootout

{-#LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where
import Transient.Internals
import Transient.Move
--import Transient.EVars
import Control.Applicative
import Transient.Logged
import Transient.Move.Utils
--import Data.Text hiding (empty)
import Control.Monad.IO.Class


import qualified Data.Aeson as Aeson
import qualified Network.WebSockets.Connection   as WS
import qualified Data.ByteString.Lazy.Char8  as BS
import Data.Containers
import System.IO.Unsafe

import System.Mem.StableName

import Control.Concurrent
import Data.IORef
import qualified Data.Map as M
import Control.Exception
import Control.Monad
import qualified Control.Concurrent.Chan.Unagi as Unagi

rmap= unsafePerformIO $ newIORef M.empty

data Msg = Echo | Broadcast BS.ByteString

main= keep' . freeThreads $ do
  broad <- newEVar
--  clients  <- liftIO $ newIORef [] -- (M.empty)
  initNode $ apisample    broad



apisample5 clients = Cloud $ do
    Connection _(Just (Node2Web  conn )) _ _ _ _ _ _ <- getSData <|> error "ERRROR"
    msg <- paramVal
    processMessage conn msg
 <|>   do
     Connection _(Just (Node2Web  conn )) _ _ _ _ _ _ <- getSData <|> error "ERRROR"
     liftIO . atomicModifyIORef clients $ \m -> (  conn :m , ())
 where
 processMessage conn msg= do
     case parseMsg msg of
--          Nothing -> error "NOTHING"  -- WS.sendClose conn ("Invalid message" :: BS.ByteString)

          Just Echo -> liftIO $ WS.sendTextData conn msg

          Just (Broadcast res) -> do

            cs <- liftIO $ readIORef clients
            liftIO $ mapM (flip WS.sendTextData msg)  cs -- !> (length cs)
            liftIO $ WS.sendTextData conn res


parseMsg :: BS.ByteString -> Maybe Msg
parseMsg msg = do
  Aeson.Object obj <- Aeson.decode msg
  Aeson.String typ <- Data.Containers.lookup "type" obj

  case typ of
    "echo" -> Just Echo

    "broadcast" -> let
      res = Aeson.encode (insertMap "type" "broadcastResult" obj)
      in Just (Broadcast res)

    _ -> Nothing


apisample broad= api $

    do       msg <- paramVal
             processMessage broad msg
        <|>  watchBroadcast broad



processMessage broad msg= do
    Aeson.Object obj <- emptyIfNothing $ Aeson.decode msg
    Aeson.String typ <- emptyIfNothing $ Data.Containers.lookup "type" obj
    case typ of
       "echo" -> return msg
       "broadcast" ->  do
                  let res = Aeson.encode  $ insertMap "type" "broadcastResult" obj
                  writeEVar broad msg
                  return res


watchBroadcast broad= threads 0 $ readEVar broad

emptyIfNothing= Transient . return


data EVar a= EVar  (Unagi.InChan ( StreamData a))

readEVar :: EVar a -> TransIO a
readEVar (EVar ref1)=  do
     tchan <-  liftIO $  Unagi.dupChan  ref1
     mx <-  parallel $   Unagi.readChan tchan `catch` \(e :: SomeException) -> error $ show e
     case mx of
      SError e -> finish $ Just e
      SMore x -> return x


newEVar :: TransIO (EVar a)
newEVar  = Transient $ do
   (ref, _) <- liftIO $  Unagi.newChan
   return . Just $ EVar  ref

writeEVar (EVar  ref1) x= liftIO  $ do
       Unagi.writeChan  ref1 $ SMore x


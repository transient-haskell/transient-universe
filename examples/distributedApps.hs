{-# LANGUAGE   CPP   #-}

module Main where

import Prelude hiding (div,id)
import Transient.Internals ((!>))
import Transient.Base

#ifdef ghcjs_HOST_OS
   hiding ( option)
#endif
import GHCJS.HPlay.Cell
import GHCJS.HPlay.View
#ifdef ghcjs_HOST_OS
   hiding (map,input)
#else
   hiding (map, option,input)
#endif


import Transient.Move
import Control.Applicative
import Control.Monad
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Map as M
import Transient.MapReduce
import Control.Monad.IO.Class
import System.IO
import Data.String




main =  do

    keep $ do
       port <- getPort
       initWebApp port $ mapReduce <|> chat <|> addNode

getPort :: TransIO Integer
getPort =
      if isBrowserInstance then return 0 else do
          oneThread $ option "start" "re/start"
          liftIO $ putStr "port to listen? " >> hFlush stdout
          port <- input (const True)
          liftIO $ putStrLn "node started"
          return port

addNode= when (not isBrowserInstance) $ (do
          local $ option "add"  "add a new node at any moment"
          lliftIO $ putStr "Host to connect to: (none): " >> hFlush stdout
          host <- local $ do
                    r <- input $ const True
                    liftIO $ print r
                    if r ==  "" then stop else return r
          lliftIO $ putStr "port? " >> hFlush stdout
          port <-  local $ input $ const True
          mynode <- local getMyNode
          connect' mynode  $ createNode host port)
        <|> return ()




-- A Web node launch a map-reduce computation in all the server nodes, getting data from a
-- textbox and render the results returned

mapReduce= do
  server <- onAll getSData

  wormhole server $ do

    content <-  local . render $
                    textArea   (fs "") ! atr "placeholder" (fs "enter the content")
                                      ! atr "rows"  (fs "4")
                                      ! atr "cols"  (fs "80")

                     <++ br
                     <*** inputSubmit "send" `fire` OnClick
                     <++ br

    r <- atRemote $ do
               lliftIO $ print content
               r<- reduce  (+) . mapKeyB (\w -> (w, 1 :: Int))  $ distribute  $ V.fromList $ words content
               lliftIO $ print r
               return (r :: M.Map String Int)


    local . render $ rawHtml $do
                 h1 "Results"
                 mconcat[i "word " >> b w >> i " appears " >> b n >> i "times" >> br
                        | (w,n) <- M.assocs r]


fs= fromString


chat= do
  server <- onAll $ getSData <|> error "server not set"
  wormhole server $ do
    local . render . rawHtml $ div ! id (fs "chatbox")
                                   ! style (fs "overflow: auto; max-height: 200px;")
                                   $ noHtml  -- create the chat box
    sendMessages <|> waitMessages

  where
  sendMessages= do
--      text <- local . render $ (inputString Nothing )  `fire` OnChange
--                <*** inputSubmit "send"
--                <++ br
      let entry= boxCell (fs "msg")
      text <- local . render $ (mk entry Nothing )  `fire` OnChange
                <*** inputSubmit "send"
                <++ br
      local $ entry .= ""
      teleport      -- move  to the server
      clustered . local $ putMailBox "chat" (text :: String)  -- write in all the server mailboxes
                                                              -- This is INEFFICIENT:
                                                              --  for each user/message
                                                              --   a new connection & message to
                                                              --    each server
      stop

  waitMessages= do
    resp <- atRemote . local $ getMailBox "chat" :: Cloud String  -- wait in the server for messages
    local . render . at (fs "#chatbox") Append $ rawHtml $ p resp   -- append it to the chat box









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
import Transient.Indeterminism
import Transient.EVars
import Control.Applicative
import Control.Monad
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Map as M
import Transient.MapReduce
import Control.Monad.IO.Class
import System.IO
import Data.String
import qualified Data.Text as T
#ifdef ghcjs_HOST_OS
import qualified Data.JSString as JS hiding (span,empty,strip,words)
#endif


main =  keep $ do
       port <- getPort
       initWebApp port $  do

          server <- onAll $ getSData <|> error "server not set"
          wormhole server $ onBrowser mapReduce <|> onBrowser  chat <|> onServer addNode

getPort :: TransIO Integer
getPort =
      if isBrowserInstance then return 0 else do
          oneThread $ option "start" "re/start"
          port <- input (const True) "port to listen? "
          liftIO $ putStrLn "node started"
          return port

addNode=do
   onServer $ do
          local $ option "add"  "add a new node at any moment"

          host <- local $ do
                    r <- input (const True) "Host to connect to: (none): "
                    if r ==  "" then stop else return r

          port <-  local $ input (const True) "port?"

          connectit <- local $ input (\x -> x=="y" || x== "n") "connect to get his list of nodes?"
          let nnode= createNode host port
          if connectit== "y" then connect'  nnode
                             else local $ addNodes [nnode]
   empty



-- A Web node launch a map-reduce computation in all the server nodes, getting data from a
-- textbox and render the results returned

mapReduce= do

    content <-  local . render $
                    textArea   (fs "") ! atr "placeholder" (fs "enter the content")
                                       ! atr "rows"  (fs "4")
                                       ! atr "cols"  (fs "80")

                     <++  br
                     <*** inputSubmit "send" `fire` OnClick
                     <++  br

    r <- atRemote $ do
               lliftIO $ print content
               r<- reduce  (+) . mapKeyB (\w -> (w, 1 :: Int))  $ getText words content
               lliftIO $ putStr ">>>>>>>>>>>>>>" >> print r
               return (r :: M.Map String Int)


    local . render $ rawHtml $do
                 h1 "Results"
                 mconcat[i "word " >> b w >> i " appears " >> b n >> i " times" >> br
                        | (w,n) <- M.assocs r]


fs= fromString


chat=   do
    let chatMessages= T.pack "chatMessages"

    local . render . rawHtml $ div ! id (fs "chatbox")
                                   ! style (fs "margin-top:1cm;overflow: auto;height: 200px;background-color: #FFCC99; max-height: 200px;")
                                   $ noHtml  -- create the chat box

    sendMessages chatMessages <|>  waitMessages chatMessages

  where
  sendMessages chatMessages = do
      let entry= boxCell (fs "msg") ! atr "size"  (fs "90")
      text <- local . render $ (mk entry Nothing )  `fire` OnChange
                <*** inputSubmit "send"
                <++ br
      local $ entry .= ""

      atRemote $ do
          node <- local getMyNode
          clustered $ local $ putMailbox chatMessages (show node ++ text :: String)
          return ()


  waitMessages chatMessages = do
    resp <- atRemote . local $  getMailbox chatMessages  -- atRemote, in the server
                                                           -- wait in the server for messages

    local . render . at (fs "#chatbox") Append $ rawHtml $ do
                                          p (resp :: String)    -- display the response
#ifdef ghcjs_HOST_OS
                                          liftIO $ scrollBottom $ fs "chatbox"

foreign import javascript unsafe
  "var el= document.getElementById($1);el.scrollTop=  el.scrollHeight"
  scrollBottom  :: JS.JSString -> IO()
#endif

-- only execute if is the browser but can call the server:
onBrowser x= do
     r <- local $  return isBrowserInstance
     if r then x else empty

onServer x= do
     r <- local $  return isBrowserInstance
     if not r then x else empty





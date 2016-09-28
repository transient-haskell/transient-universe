{-# LANGUAGE   CPP, NoMonomorphismRestriction, DeriveDataTypeable  #-}

module Main where

import Prelude hiding (div,id)
import Transient.Internals



import GHCJS.HPlay.Cell
import GHCJS.HPlay.View
#ifdef ghcjs_HOST_OS
   hiding (map, input,option)
#else
   hiding (map, option,input)
#endif


import Transient.Move
import Transient.EVars
import Transient.Indeterminism

import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Map as M
import Transient.MapReduce
import Control.Monad.IO.Class
import Data.String
import qualified Data.Text as T

#ifdef ghcjs_HOST_OS
import qualified Data.JSString as JS hiding (span,empty,strip,words)
#endif

import Data.Typeable





data Options= MapReduce | Chat | MonitorNodes | AllThree deriving (Typeable, Read, Show)

main =  keep' $  initNode  $ inputNodes <|> do
     op <- local .  render $
                wlink MapReduce (b "map-reduce") <++ fs "   " <|>
                wlink Chat (b "chat") <++ fs "   " <|>
                wlink MonitorNodes (b "monitor nodes") <++ fs "   " <|>
                wlink AllThree  (b "all widgets")

     case op of
       AllThree -> allw
       MapReduce -> mapReduce
       Chat -> chat
       MonitorNodes -> monitorNodes


allw=   mapReduce <|> chat  <|>  monitorNodes




-- A Web node launch a map-reduce computation in all the server nodes, getting data from a
-- textbox and render the results returned

mapReduce= onBrowser $ do

    content <- local . render $
            h1 "Map-Reduce widget" ++>
            p "Return the frequencies of words from a text using all the server nodes connected" ++>
            textArea   (fs "") ! atr "placeholder" (fs "enter the content")
                               ! atr "rows"  (fs "4")
                               ! atr "cols"  (fs "80")
            <++  br
            <**  inputSubmit "send"  `fire` OnClick
            <++  br

    r <- atRemote $ do
           lliftIO $ print content
           r<- reduce  (+) . mapKeyB (\w -> (w, 1 :: Int))  $ distribute $ V.fromList $ words content
           lliftIO $ putStr "result:" >> print r
           return   (r :: M.Map String Int)


    local . render $ rawHtml $ do
             h1 "Results"
             mconcat[i "word " >> b w >> i " appears " >> b n >> i " times" >> br
                    | (w,n) <- M.assocs r]

    empty

fs= fromString

-- a chat widget that run in the browser and in a cloud of servers





chat = do

    let chatMessages= fs "chatMessages"

    local . render . rawHtml $ do
            h1 "Federated chat server"
            div ! id (fs "chatbox")
                ! style (fs $"overflow: auto;height: 200px;"
                         ++  "background-color: #FFCC99; max-height: 200px;")
                $ noHtml  -- create the chat box

    sendMessages  chatMessages <|>  waitMessages chatMessages

  where


  sendMessages  chatMessages = do
--      node <- atRemote $ local getMyNode
      let entry= boxCell (fs "msg") ! atr "size"  (fs "60")
      (nick,text) <- local . render $  (,) <$> getString (Just "anonymous") ! size (fs "10")
                                           <*> mk entry Nothing  `fire` OnChange
                                           <** inputSubmit "send"
                                           <++ br
      local $ entry .= ""

      atRemote $ do
          node <- local getMyNode

          clustered $ local $ putMailbox chatMessages (showPrompt nick node ++ text )  >> empty :: Cloud ()
          empty

      where
      fs= fromString
      size= atr (fs "size")
      showPrompt u (Node h p _ _)= u ++ "@" ++ h ++ ":" ++ show p ++ "> "

  waitMessages chatMessages =   do

      resp <- atRemote . local $ single $   getMailbox chatMessages
                                                           -- wait in the server for messages

      local . render . at (fs "#chatbox") Append $ rawHtml $ do
                                          p (resp :: String)    -- display the response
#ifdef ghcjs_HOST_OS
                                          liftIO $ scrollBottom $ fs "chatbox"


foreign import javascript unsafe
  "var el= document.getElementById($1);el.scrollTop=  el.scrollHeight"
  scrollBottom  :: JS.JSString -> IO()
#endif

monitorNodes= do
    local . render $ rawHtml $ do
         h1 "Nodes connected"
         div ! atr (fs "id") (fs "nodes") $ noHtml

    nodes <- atRemote . local $ single $  sample getNodes 1000000

    local . render . at (fs "#nodes") Insert . rawHtml $
           table $ mconcat[tr $ td h >> td p >> td s | Node h p _ s  <- nodes]
    empty









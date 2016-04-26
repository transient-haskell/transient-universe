{-# LANGUAGE   CPP,  OverloadedStrings #-}

module Main where

import Prelude hiding (div,id)
import Transient.Internals ((!>))
import Transient.Base

#ifdef ghcjs_HOST_OS
   hiding ( option)
#endif
import GHCJS.HPlay.View
#ifdef ghcjs_HOST_OS
   hiding (map,input)
#else
   hiding (map, option,input)
#endif


import Transient.Move
import Transient.Logged
import Transient.Indeterminism(choose)
import Control.Applicative
import Control.Monad
import Data.Typeable

import qualified Data.Map as M


import Transient.MapReduce


import System.Environment
import Control.Monad.IO.Class


import qualified Data.Vector as V
import System.IO
import System.IO.Unsafe

import Data.String

default (String)


-- A Web node launch a map-reduce computation in all the server nodes, getting data from a
-- textbox and render the results returned

main =  do

    args <- getArgs
    let port= if (length args >= 1) then read $ args !! 0 else 8080
    simpleWebApp port $ mapReduce <|> chat <|> addNode


addNode= when (not isBrowserInstance) $ (do
          local $ option "add"  "add a new node"
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






mapReduce= do
  server <- onAll $ getSData
  wormhole server $ do

    content <-  local . render $
                    textArea   "" ! atr "placeholder" "enter the content"
                                      ! atr "rows" "4"
                                      ! atr "cols"  "80"

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





chat= do
  server <- onAll $ getSData <|> error "server not set"
  wormhole server $ do
    local . render . rawHtml $ div ! id "chatbox" $ noHtml
    sendMessages <|> waitMessages

  where
  sendMessages= do
      text <- local . render $ (inputString Nothing )  `fire` OnChange
                <*** inputSubmit "send"
                <++ br
      teleport      -- move it to the server
      clustered . local $ putMailBox "chat"  (text :: String)
      stop

  waitMessages= do
    resp <- atRemote . local $ getMailBox "chat" :: Cloud String
    local . render . at "#chatbox" Prepend $ rawHtml $ p resp









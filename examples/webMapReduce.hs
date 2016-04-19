{-# LANGUAGE   CPP #-}

module Main where

import Prelude hiding (div,id)
import Transient.Internals ((!>))
import Transient.Base

#ifdef ghcjs_HOST_OS
   hiding ( option)
#endif
import GHCJS.HPlay.View
#ifdef ghcjs_HOST_OS
   hiding (map)
#else
   hiding (map, option)
#endif

import Transient.Move
import Transient.Logged
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import qualified Data.Vector as V
import qualified Data.Map as M

#ifndef ghcjs_HOST_OS
import Transient.MapReduce
--message= lliftIO $ print "hello"  >> return (M.fromList [("hello",1::Int)])

#else

-- dummy Transient.MapReduce module
reduce _ _ = local stop :: Loggable a => Cloud a
mapKeyB _ _= undefined
distribute _ = undefined
getText _ _ = undefined
eval _= undefined
data PartRef a=PartRef a
#endif


-- Show the composability of transient web aplications
-- with three examples composed together, each one is a widget that execute
-- code in the browser AND the server.

main =   simpleWebApp 8080 app


app= do

  server <- onAll $ getSData
  wormhole server $ do

    content <-  local . render $
                    textArea  (fs "") ! atr "placeholder" (fs "enter the content")
                                      ! atr "rows" (fs "4")
                                      ! atr "cols" (fs "80")
                                 `fire` OnClick
                     <++ br
                     <*** inputSubmit "send" `fire` OnClick

    r <- atRemote $ do
               lliftIO $ print content
               r<- reduce  (+)
                    . mapKeyB (\w -> (w, 1 :: Int))
                    $ getText  words content
               lliftIO $ print r
               return (r :: M.Map String Int)


    local . render $ rawHtml $do
                 h1 "Results"
                 mconcat[i "word " >> b w >> i " appears " >> b n >> i "times" >> br
                        | (w,n) <- M.assocs r]

fs= toJSString




{-
#ifndef ghcjs_HOST_OS
test= loggedc $ do
                 server <- local getSData
                 runAt server $ local $ return $ M.fromList [("pepe",1)]
#else
test= local stop
#endif
-}

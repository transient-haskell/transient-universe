-----------------------------------------------------------------------------
--
-- Module      :  Transient.Move.Utils
-- Copyright   :
-- License     :  GPL-3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Transient.Move.Utils (initNode,inputNodes, simpleWebApp, initWebApp
, onServer, onBrowser, runTestNodes)
 where

import Transient.Base
--import Transient.Internals((!>))
import Transient.Move
import Control.Applicative
import Control.Monad.IO.Class
import Data.IORef

-- | ask in the console for the port number and initializes a node in the port specified
-- It needs the application to be initialized with `keep` to get input from the user.
-- the port can be entered in the command line with "<program> -p  start/<PORT>"
--
-- A node is also a web server that send to the browser the program if it has been
-- compiled to JavaScript with ghcjs. `initNode` also initializes the web nodes.
--
-- This sequence compiles to JScript and executes the program with a node in the port 8080
--
-- > ghc program.hs
-- > ghcjs program.hs -o static/out
-- > ./program -p start/8080
--
-- `initNode`, when the application has been loaded and executed in the browser, will perform a `wormhole` to his server node.
--  So the application run within this wormhole.
--
--  Since the code is executed both in server node and browser node, to avoid confusion and in order
-- to execute in a single logical thread, use `onServer` for code that you need to execute only in the server
-- node, and `onBrowser` for code that you need in the browser, although server code could call the browser
-- and vice-versa.
--
-- To invoke from browser to server and vice-versa, use `atRemote`.
--
-- To translate the code from the browser to the server node, use `teleport`.
--
initNode :: Cloud () -> TransIO ()
initNode app= do
   node <- getPort
   initWebApp node  app


  where
  getPort :: TransIO Node
  getPort =
      if isBrowserInstance then  liftIO createWebNode else do
          oneThread $ option "start" "re/start node"
          host <- input (const True) "hostname of this node (must be reachable): "
          port <- input (const True) "port to listen? "
          liftIO $ createNode host port

-- | ask for nodes to be added to the list of known nodes. it also ask to connect to the node to get
-- his list of known nodes
inputNodes= do
   onServer $ do
          local $ option "add"  "add a new node at any moment"

          host <- local $ do
                    r <- input (const True) "Host to connect to: (none): "
                    if r ==  "" then stop else return r

          port <-  local $ input (const True) "port?"

          connectit <- local $ input (\x -> x=="y" || x== "n") "connect to get his list of nodes?"
          nnode <- localIO $ createNode host port
          if connectit== "y" then connect'  nnode
                             else local $ addNodes [nnode]
   empty



-- | executes the application in the server and the Web browser.
-- the browser must point to http://hostname:port where port is the first parameter.
-- It creates a wormhole to the server.
-- The code of the program after `simpleWebApp` run in the browser unless `teleport` translates the execution to the server.
-- To run something in the server and get the result back to the browser, use  `atRemote`
-- This last also works in the other side; If the application was teleported to the server, `atRemote` will
-- execute his parameter in the browser.
--
-- It is necesary to compile the application with ghcjs:
--
-- > ghcjs program.js
-- > ghcjs program.hs -o static/out
--
-- > ./program
--
--
simpleWebApp :: Integer -> Cloud () -> IO ()
simpleWebApp port app = do
   node <- createNode "localhost" port
   keep $ initWebApp node app

-- | use this instead of smpleWebApp when you have to do some initializations in the server prior to the
-- initialization of the web server
initWebApp :: Node -> Cloud () -> TransIO ()
initWebApp node app=  do
    conn <- defConnection
    setData  conn{myNode = node}
    serverNode  <-   getWebServerNode  :: TransIO Node

    mynode <- if isBrowserInstance
                    then liftIO $ createWebNode
                    else return serverNode

    runCloud $ do
        listen mynode <|> return()
        wormhole serverNode app   -- !> ("servernode", serverNode)
        return ()

-- only execute if the the program is executing in the browser. The code inside can contain calls to the server.
-- Otherwise return empty (so it stop the computation).
onBrowser :: Cloud a -> Cloud a
onBrowser x= do
     r <- local $  return isBrowserInstance
     if r then x else empty

-- only executes the computaion if it is in the server, but the computation can call the browser. Otherwise return empty
onServer :: Cloud a -> Cloud a
onServer x= do
     r <- local $  return isBrowserInstance
     if not r then x else empty

-- | run N nodes (N ports to listen) in the same program. For testing purposes.
-- It add them to the list of known nodes, so it is possible to perform `clustered` operations with them.
runTestNodes ports= do
    nodes <- onAll $  mapM (\p -> liftIO $ createNode "localhost" p) ports
    foldl (<|>) empty (map listen nodes) <|> return()


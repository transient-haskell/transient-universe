#!/usr/bin/env execthirdlinedocker.sh
--  info: use sed -i 's/\r//g' file if report "/usr/bin/env: ‘execthirdlinedocker.sh\r’: No such file or directory"
-- runghc    -i../transient/src -i../transient-universe/src -i../axiom/src   $1 ${2} ${3}

--  mkdir -p ./static && ghcjs --make   -i../transient/src -i../transient-universe/src  -i../axiom/src -i../ghcjs-perch/src $1 -o static/out && runghc   -i../transient/src -i../transient-universe/src -i../axiom/src   $1 ${2} ${3}


module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           System.Environment
import           System.IO
import           Transient.Base
import           Transient.Indeterminism
import           Transient.Logged
import           Transient.Move
import           Transient.Move.Utils
import           GHCJS.HPlay.View  hiding (input)
import           Control.Applicative
import           System.Info
import           Control.Concurrent

main = keep $ rerun "config" $ do
  logged $ liftIO $ do
     putStrLn "configuring the program"
     putStrLn "The program will not ask again in further executions within this folder"
  
  host <- logged $ input (const True)  "host? "
  port <- logged $ input (const True)  "port? "
  checkpoint
  
  liftIO $ putStrLn $ "Running server at " ++ host ++ ":" ++ show port
  node <- liftIO $ createNode host port
  initWebApp node $ do 

     local $ render $ rawHtml $ p "Hello world"
  